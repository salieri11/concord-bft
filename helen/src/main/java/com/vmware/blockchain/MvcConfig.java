/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;

import com.fasterxml.jackson.databind.PropertyNamingStrategy;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.datatype.jsr310.ser.ZonedDateTimeSerializer;

/**
 * Configurations for Spring MVC, such as default serializers.
 */
@Configuration
public class MvcConfig {
    public static final String ISO_8601_SIMPLE_DATE_PATTERN = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'";


    /**
     * Override the default Object Builder for spring calls.  Our default is snake case.
     * @return builder
     */
    @Bean
    public Jackson2ObjectMapperBuilder jacksonBuilder() {
        Jackson2ObjectMapperBuilder b = new Jackson2ObjectMapperBuilder();
        b.propertyNamingStrategy(PropertyNamingStrategy.SNAKE_CASE);
        JavaTimeModule javaTimeModule = new JavaTimeModule();
        javaTimeModule.addSerializer(ZonedDateTime.class,
                new ZonedDateTimeSerializer(DateTimeFormatter.ofPattern(ISO_8601_SIMPLE_DATE_PATTERN)));
        b.modulesToInstall(javaTimeModule);
        return b;
    }

}
