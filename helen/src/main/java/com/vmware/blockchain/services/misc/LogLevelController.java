/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.misc;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.config.Configuration;
import org.apache.logging.log4j.core.config.Configurator;
import org.apache.logging.log4j.core.config.LoggerConfig;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.Data;


/**
 * Rest controller for application log.
 * Based on https://logging.apache.org/log4j/log4j-2.4/faq.html#set_logger_level_from_code
 */
@RestController
public class LogLevelController {
    private static final Logger log = LogManager.getLogger(LogLevelController.class);

    private static ResetLogLevelRunnable runnable;
    private static Thread runner;
    // log level will be reset in 5 minutes.
    private static final AtomicLong RESET_LOG_LEVEL_IN_MS = new AtomicLong(5 * 60 * 1000L);
    // Sleep for 10 seconds.
    private static final AtomicLong THREAD_SLEEP_TIME_MS = new AtomicLong(10 * 1000L);
    // Original global log level before the reset.
    private static Level origGlobalLogLevel;
    // Original log level for a logger before the reset.
    private static final Map<String, Level> origLogLevels = new HashMap<>();

    private static final String RESPONSE_LOGGERS_KEY = "loggers";
    private static final String RESPONSE_LOGGER_NAME_KEY = "logger";
    private static final String RESPONSE_LOG_LEVEL_KEY = "log_level";
    private static final String INVALID_LOG_LEVEL_MSG = "Invalid Log level";
    private static final String INVALID_LOGGER_MSG = "Invalid Logger";
    private static final String INVALID_RESET_TO_DEFAULT_VALUE_MSG = "Invalid Logger";
    private static final String ERROR_KEY = "error";
    private static final Integer RESET_TO_DEFAULT_MAX_VALUE_IN_MINS = 20;
    private static final Integer RESET_TO_DEFAULT_MIN_VALUE_IN_MINS = 2;

    @Autowired
    public LogLevelController() {
        init();
    }

    private void init() {
        // Get global log level.
        origGlobalLogLevel = getCurrentLogLevel();
        log.info("Original log level captured " + origGlobalLogLevel);
        // Get all available loggers.
        List<String> loggers = getLoggerNames();
        loggers.forEach(loggerName -> {
            origLogLevels.put(loggerName, getCurrentLogLevel(loggerName));
            log.info("Original logger log level captured " + origLogLevels.get(loggerName));
        });
    }

    /**
     * Get a list of loggers defined in the system.
     * @return List of loggers.
     */
    @RequestMapping(path = "/api/config/logging/logger", method = RequestMethod.GET)
    @PreAuthorize("@authHelper.isSystemAdmin()")
    ResponseEntity<Map<String, List<String>>> getLoggers() {
        List<String> loggerList = getLoggerNames();
        log.info("logger list " + loggerList);
        var responseMap = new HashMap<String, List<String>>();
        responseMap.put(RESPONSE_LOGGERS_KEY, loggerList);
        return new ResponseEntity<>(responseMap, HttpStatus.OK);
    }

    /**
     * Get current log level:
     * If the request includes a logger name, then return log level for this logger.
     * If the request did not include a logger, then return log level for first available logger.
     * @return log level.
     */
    @RequestMapping(path = "/api/config/logging/log_level", method = RequestMethod.GET)
    @PreAuthorize("@authHelper.isSystemAdmin()")
    ResponseEntity<Map<String, String>> getLogLevel(@RequestParam(required = false) String logger) {
        // Is the request for a logger? if yes, then return log level for the logger.
        if (logger != null && !logger.isEmpty()) {
            boolean isValidLogger = isValidLogger(logger);
            if (!isValidLogger) {
                log.error("Invalid logger value {}", logger);
                return new ResponseEntity<>(Map.of(ERROR_KEY, INVALID_LOGGER_MSG + " " + logger),
                                            HttpStatus.BAD_REQUEST);
            }
            Level logLevel = getCurrentLogLevel(logger);
            return new ResponseEntity<>(getLogLevelResponse(logLevel, logger), HttpStatus.OK);
        }
        Level logLevel = getCurrentLogLevel();
        return new ResponseEntity<>(getLogLevelResponse(logLevel), HttpStatus.OK);
    }

    /**
     * Set the log level to the supplied logger.
     * If no logger is specified, set global log level.
     * @param body request body
     * @return Response with log level and optionally logger.
     */
    @RequestMapping(path = "/api/config/logging/log_level", method = RequestMethod.PUT)
    @PreAuthorize("@authHelper.isSystemAdmin()")
    ResponseEntity<Map<String, String>> putLogLevel(@RequestBody LogLevelPut body) {
        // Are we setting log level to a logger?
        var loggerName = body.getLogger();
        Level[] supportedLogLevels = Level.values();
        String logLevelStr = body.getLogLevel();
        Level logLevel = null;
        for (Level level : supportedLogLevels) {
            if (level.name().equalsIgnoreCase(logLevelStr)) {
                logLevel = level;
                break;
            }
        }
        if (logLevel == null) {
            log.error("Invalid log level value {}", logLevelStr);
            return new ResponseEntity<>(Map.of(ERROR_KEY, INVALID_LOG_LEVEL_MSG + " " + logLevelStr),
                                                           HttpStatus.BAD_REQUEST);
        }

        if (loggerName != null && !loggerName.isEmpty()) {
            List<String> loggerNames = getLoggerNames();
            boolean isLoggerNameValid = isValidLogger(loggerName);
            if (!isLoggerNameValid) {
                log.error("Invalid logger name {}", loggerName);
                return new ResponseEntity<>(Map.of(ERROR_KEY, INVALID_LOGGER_MSG + " " + loggerName),
                                                               HttpStatus.BAD_REQUEST);
            }
        }
        var resetToDefaultValue = body.getResetToDefaultInMins();
        if (resetToDefaultValue != null && (resetToDefaultValue < RESET_TO_DEFAULT_MIN_VALUE_IN_MINS
                                            || resetToDefaultValue > RESET_TO_DEFAULT_MAX_VALUE_IN_MINS)) {
            log.error("Invalid reset to default value {}", resetToDefaultValue);
            return new ResponseEntity<>(
                    Map.of(ERROR_KEY, INVALID_RESET_TO_DEFAULT_VALUE_MSG + " " + resetToDefaultValue),
                    HttpStatus.BAD_REQUEST);
        }

        // Set/reset the log level.
        var responseMap = resetLogLevel(logLevel, loggerName, resetToDefaultValue);
        return new ResponseEntity<>(responseMap, HttpStatus.OK);
    }

    /**
     * Reset log level for a given logger, or if no logger is supplied, then at global level.
     * @param logLevel log level
     * @param loggerName logger name
     * @return A response map with changed log level.
     */
    private synchronized Map<String, String> resetLogLevel(Level logLevel, String loggerName,
                                                           Integer resetToDefaultValue) {
        // Check to see if the thread is still active.
        // If it is, then reset the expiration time.
        if (runner != null && runner.isAlive()) {
            log.debug("Thread is already running with state " + runner.getState().name()
                         + ", so reset the timer and log level on it.");
            long expirationTime =
                    System.currentTimeMillis() + (resetToDefaultValue != null ? resetToDefaultValue * 60 * 1000L
                                                                              : RESET_LOG_LEVEL_IN_MS.get());
            log.debug("Resetting the expiration time to " + new Date(expirationTime).toString()
                         + " and log level to " + logLevel);
            runnable.resetLogLevel(logLevel, loggerName);
            runnable.setExpirationTime(expirationTime);
            var responseMap = new HashMap<String, String>();
            responseMap.put(RESPONSE_LOG_LEVEL_KEY, logLevel.name());
            if (loggerName != null && !loggerName.isEmpty()) {
                responseMap.put(RESPONSE_LOGGER_NAME_KEY, loggerName);
            }
            return responseMap;
        }

        setLogLevel(logLevel, loggerName);

        long expirationTime =
                System.currentTimeMillis() + (resetToDefaultValue != null ? resetToDefaultValue * 60 * 1000L
                                                                          : RESET_LOG_LEVEL_IN_MS.get());
        runnable = new ResetLogLevelRunnable(expirationTime, loggerName);
        runner = new Thread(runnable);

        TerminateResetLogLevelRunnable shutDownResetLogLevelRunnable = new TerminateResetLogLevelRunnable(runnable);
        Runtime.getRuntime().addShutdownHook(new Thread(shutDownResetLogLevelRunnable));

        log.debug("Start the runner thread with expiration time " + new Date(expirationTime));
        runner.start();

        var responseMap = new HashMap<String, String>();
        responseMap.put(RESPONSE_LOG_LEVEL_KEY, logLevel.name());
        if (loggerName != null && !loggerName.isEmpty()) {
            responseMap.put(RESPONSE_LOGGER_NAME_KEY, loggerName);
        }
        return responseMap;
    }

    /**
     * Set the log level for the given logger, if no loggerName supplied, then set the global log level.
     * @param level log level
     * @param loggerName logger name, or null if global
     */
    private synchronized void setLogLevel(Level level, String loggerName) {
        if (loggerName != null && !loggerName.isEmpty()) {
            if (getCurrentLogLevel(loggerName) == level) {
                log.info("Logger {} Log level currently is {}. Ignore the request.", loggerName, level);
                return;
            }
            log.info("Setting/Resetting log level for logger {} loggers to {}", loggerName, level);
            Configurator.setLevel(loggerName, level);
        } else {
            if (getCurrentLogLevel() == level) {
                log.info("Log level currently is " + level + ". Ignore the request.");
                return;
            }
            log.info("Setting/Resetting log level for all loggers to {}", level);
            LoggerContext context = (LoggerContext) LogManager.getContext(false);
            Configuration config = context.getConfiguration();
            Map<String, LoggerConfig> loggers = config.getLoggers();
            Configurator.setRootLevel(level);
            for (String name : loggers.keySet()) {
                Configurator.setLevel(name, level);
            }
        }
    }

    /**
     * Convert the log level to response map.
     * @param level log level
     * @return A response map.
     */
    private Map<String, String> getLogLevelResponse(Level level) {
        return getLogLevelResponse(level, null);
    }

    /**
     * Convert the log level and logger name to response map.
     * @param level log level
     * @param loggerName logger name
     * @return A response map.
     */
    private Map<String, String> getLogLevelResponse(Level level, String loggerName) {
        log.info("Current log level " + level.name());
        var responseMap = new HashMap<String, String>();
        if (loggerName != null && !loggerName.isEmpty()) {
            responseMap.put(RESPONSE_LOGGER_NAME_KEY, loggerName);
        }
        responseMap.put(RESPONSE_LOG_LEVEL_KEY, level.name());
        return responseMap;
    }

    /**
     * Get names of all configured loggers.
     * @return A list of logger names.
     */
    private List<String> getLoggerNames() {
        log.debug("Get loggers.");
        var loggerNames = new ArrayList<String>();
        LoggerContext context = (LoggerContext) LogManager.getContext(false);
        Configuration config = context.getConfiguration();
        Map<String, LoggerConfig> loggers = config.getLoggers();
        for (String loggerName : loggers.keySet()) {
            log.debug("loggerName " + loggerName + " " + loggers.get(loggerName).getParent() + " "
                               + loggers.get(loggerName).getLevel() + " " + loggers.get(loggerName).getPropertyList()
                               + " " + loggers.get(loggerName).isAdditive());
            // Suppress root logger
            if (loggerName != null && !loggerName.isEmpty()) {
                loggerNames.add(loggerName);
            }
        }
        return loggerNames;
    }

    /**
     * Get the current log level.
     * @return Level log level.
     */
    public Level getCurrentLogLevel() {
        return getCurrentLogLevel(null);
    }

    /**
     * Get the current log level of a logger.
     * @return Level log level.
     */
    public Level getCurrentLogLevel(String loggerName) {
        Level currLogLevel = null;
        LoggerContext context = (LoggerContext) LogManager.getContext(false);
        Configuration config = context.getConfiguration();
        Map<String, LoggerConfig> loggers = config.getLoggers();
        log.debug("Current log level setting for the loggers:");
        for (String name : loggers.keySet()) {
            log.debug("loggerName " + name + " " + loggers.get(name).getParent() + " "
                               + loggers.get(name).getLevel() + " " + loggers.get(name).getPropertyList()
                               + " " + loggers.get(name).isAdditive());
            // If loggerName is available, then use it.
            if (loggerName != null && loggerName.equalsIgnoreCase(name)) {
                return loggers.get(loggerName).getLevel();
            }
            // Get log level for logger with name 'name'.
            currLogLevel = loggers.get(name).getLevel();
        }
        return currLogLevel;
    }

    /**
     * Input object for PUT request.
     */
    @Data
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private static class LogLevelPut {
        String logLevel;
        String logger;
        Integer resetToDefaultInMins;
    }

    /**
     * Check if the logger is valid.
     * @param logger Name of the logger
     * @return True if the logger is valid, false otherwise.
     */
    private boolean isValidLogger(String logger) {
        boolean isValidLogger = false;
        List<String> loggerNames = getLoggerNames();
        // Is the logger valid?
        for (String loggerName : loggerNames) {
            if (loggerName.equalsIgnoreCase(logger)) {
                isValidLogger = true;
                break;
            }
        }
        return isValidLogger;
    }


    /**
     * A background thread to reset the log level to its original state after certain period of time.
     * This way, we can cover the case where someone missing to reset the log level to original after use.
     */
    class ResetLogLevelRunnable implements Runnable {
        volatile long expirationTime;
        volatile Level logLevel;
        volatile AtomicBoolean terminate = new AtomicBoolean(false);
        volatile String loggerName;

        /**
         * Constuctor.
         * @param expirationTime expiration time for the thread.
         * @param loggerName Name of the logger (optional)
         */
        public ResetLogLevelRunnable(long expirationTime, String loggerName) {
            this.expirationTime = expirationTime;
            this.loggerName = loggerName;
        }

        /**
         * Run method where the thread does the main actions: sleep, call setLogLevel() etc.,
         */
        public void run() {
            Thread t = Thread.currentThread();
            t.setName("Thread " + System.currentTimeMillis());
            log.info("Name of the thread - " + t.getName());
            while (!terminate.get()) {
                try {
                    long currTime = System.currentTimeMillis();
                    boolean expired = (currTime >= expirationTime);
                    if (expired) {
                        log.info("Thread expiration time reached, so call reset method. Curr time "
                                    + new Date(currTime));
                        if (loggerName != null && !loggerName.isEmpty()) {
                            setLogLevel(origLogLevels.get(loggerName), loggerName);
                        } else {
                            setLogLevel(origGlobalLogLevel, null);
                        }
                        break;
                    }
                    log.trace("About to sleep for " + (THREAD_SLEEP_TIME_MS.get() / 1000) + " seconds."
                                + new Date(currTime));
                    t.sleep(THREAD_SLEEP_TIME_MS.get());
                } catch (InterruptedException ioe) {
                    log.error("Error during run ", ioe);
                }
            }
            log.info("Done with the thread: " + t.getName());
        }

        /**
         * Set/Reset the expiration time.
         * @param expirationTime expiration time in millis.
         */
        public void setExpirationTime(long expirationTime) {
            this.expirationTime = expirationTime;
            log.info("Reset the expiration time to " + new Date(expirationTime));
        }

        /**
         * Reset log level to the supplied level.
         * @param logLevel log level
         */
        public void resetLogLevel(Level logLevel, String loggerName) {
            this.logLevel = logLevel;
            log.info("Reset the log level to " + logLevel);
            setLogLevel(logLevel, loggerName);
        }

        /**
         * Set the terminate value.
         * @param terminate terminate the thread value.
         */
        public void setTerminate(AtomicBoolean terminate) {
            this.terminate = terminate;
            log.info("Setting terminate to " + terminate.get());
        }

        /**
         * Stop the currently running reset loglevel thread.
         */
        public void stop() {
            this.terminate = new AtomicBoolean(true);
            Thread.currentThread().interrupt();
        }
    }

    /**
     * Terminate the background reset log level thread during a shutdown.
     */
    class TerminateResetLogLevelRunnable implements Runnable {
        volatile ResetLogLevelRunnable runnable;

        /**
         * Default constructor.
         * @param runnable Runnable object
         */
        public TerminateResetLogLevelRunnable(ResetLogLevelRunnable runnable) {
            this.runnable = runnable;
        }

        /**
         * Run method that stops the reset thread.
         */
        public void run() {
            log.info("Set terminate to true for ResetLogLevel thread.");
            runnable.stop();
        }
    }
}
