package com.vmware.blockchain.api.test.log;
  
import java.io.File;
import java.io.IOException;
import java.util.logging.ConsoleHandler;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

/**
 * Borrowed from https://www.vogella.com/tutorials/Logging/article.html
 */
public class ApiLogger {
    static private FileHandler logFileHandler;
    static private SimpleFormatter simpleFormatter;

    static public void setup() throws IOException {
        // Get the global logger to configure it
        Logger logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

        // Suppress the logging output to the console
        Logger rootLogger = Logger.getLogger("");
        Handler[] handlers = rootLogger.getHandlers();
        if (handlers[0] instanceof ConsoleHandler) {
            rootLogger.removeHandler(handlers[0]);
        }

        logger.setLevel(Level.INFO);

        File tempDir = new File(System.getProperty("java.io.tmpdir"));
        File logFile = new File(tempDir.getAbsolutePath() + "/api_tests.log");
        logFileHandler = new FileHandler(logFile.getAbsolutePath());
        System.out.println("Logging to " + logFile.getAbsolutePath());

        // create a TXT formatter
        simpleFormatter = new SimpleFormatter();
        logFileHandler.setFormatter(simpleFormatter);
        logger.addHandler(logFileHandler);
    }
}

