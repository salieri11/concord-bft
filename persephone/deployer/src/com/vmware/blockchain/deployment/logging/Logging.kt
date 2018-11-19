/* **********************************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.deployment.logging

/**
 * Contract for a logging instance for logging at various verbosity levels.
 */
expect interface Logger {
    /**
     * Indicate whether trace level logging should be enabled.
     *
     * @return
     *   `true` if logging level should be enabled, `false` otherwise.
     */
    fun isTraceEnabled(): Boolean

    /**
     * Indicate whether debug level logging should be enabled.
     *
     * @return
     *   `true` if logging level should be enabled, `false` otherwise.
     */
    fun isDebugEnabled(): Boolean

    /**
     * Indicate whether info level logging should be enabled.
     *
     * @return
     *   `true` if logging level should be enabled, `false` otherwise.
     */
    fun isInfoEnabled(): Boolean

    /**
     * Indicate whether warning level logging should be enabled.
     *
     * @return
     *   `true` if logging level should be enabled, `false` otherwise.
     */
    fun isWarnEnabled(): Boolean

    /**
     * Indicate whether error level logging should be enabled.
     *
     * @return
     *   `true` if logging level should be enabled, `false` otherwise.
     */
    fun isErrorEnabled(): Boolean

    /**
     * Record the supplied [String] at trace level.
     */
    fun trace(log: String)

    /**
     * Record the supplied [String] at debug level.
     */
    fun debug(log: String)

    /**
     * Record the supplied [String] at info level.
     */
    fun info(log: String)

    /**
     * Record the supplied [String] at warning level.
     */
    fun warn(log: String)

    /**
     * Record the supplied [String] at error level.
     */
    fun error(log: String)
}

/**
 * Create or retrieve a [Logger] by the supplied name.
 *
 * @param[name]
 *   name of the logger.
 *
 * @return
 *   the [Logger] instance corresponding to the supplied name.
 */
expect fun getLogger(name: String): Logger

/**
 * Extension function to allocate [Logger] for any type.
 */
fun <R : Any> R.logger(): Lazy<Logger> {
    return lazy {  getLogger(this::class.qualifiedName?:"default") }
}

/**
 * Extension function to allocate [Logger] for any class reference.
 */
fun <T: Any> logger(classReference: kotlin.reflect.KClass<T>): Logger {
    return getLogger(classReference.qualifiedName?:"default")
}

/**
 * Extension function to wrap `trace()` logging to [Logger] to facilitate lazy-evaluation.
 */
inline fun Logger.trace(callback: () -> String) {
    if (isTraceEnabled) trace(callback())
}

/**
 * Extension function to wrap `debug()` logging to [Logger] to facilitate lazy-evaluation.
 */
inline fun Logger.debug(callback: () -> String) {
    if (isDebugEnabled) debug(callback())
}

/**
 * Extension function to wrap `info()` logging to [Logger] to facilitate lazy-evaluation.
 */
inline fun Logger.info(payload: () -> String) {
    if (isInfoEnabled) info(payload())
}

/**
 * Extension function to wrap `warn()` logging to [Logger] to facilitate lazy-evaluation.
 */
inline fun Logger.warn(payload: () -> String) {
    if (isWarnEnabled) warn(payload())
}

/**
 * Extension function to wrap `error()` logging to [Logger] to facilitate lazy-evaluation.
 */
inline fun Logger.error(payload: () -> String) {
    if (isErrorEnabled) error(payload())
}

/**
 * Extension function to wrap `error()` logging to [Logger] to facilitate lazy-evaluation.
 */
inline fun Logger.error(payload: () -> String, throwable: Throwable) {
    if (isErrorEnabled) error(payload(), throwable)
}
