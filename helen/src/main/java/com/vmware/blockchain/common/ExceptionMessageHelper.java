/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import java.util.Locale;

import org.springframework.context.support.ReloadableResourceBundleMessageSource;

/**
 * Message resource hellper.
 */
public class ExceptionMessageHelper {

    private static final String RESOURCE_NAME = "classpath:messages";

    private static final ReloadableResourceBundleMessageSource messageSource = messageSource();

    private static ReloadableResourceBundleMessageSource messageSource() {
        ReloadableResourceBundleMessageSource messageSource = new ReloadableResourceBundleMessageSource();
        messageSource.setBasenames(RESOURCE_NAME);
        messageSource.setCacheSeconds(3600);
        messageSource.setAlwaysUseMessageFormat(true);
        return messageSource;
    }

    /**
     * Return strings for a particular code.
     *
     * @param code The code used to retrieve localized message.
     * @param args List of arguments, part of the message.
     * @return localized string value of the code.
     */
    public static String getMessageOrErrorCode(String code, Object... args) {
        return getMessageOrErrorCodeForLocale(code, null, args);
    }

    /**
     * Return localized strings for a particular code.
     *
     * @param code The code used to retrieve localized message.
     * @param locale locale code, default to US.
     * @param args List of arguments, part of the message.
     * @return localized string value of the code.
     */
    public static String getMessageOrErrorCodeForLocale(String code, Locale locale, Object... args) {
        if (locale == null) {
            locale = Locale.US;
        }

        try {
            return messageSource.getMessage(code, args, locale);
        } catch (Exception e) {
            return code;
        }
    }
}