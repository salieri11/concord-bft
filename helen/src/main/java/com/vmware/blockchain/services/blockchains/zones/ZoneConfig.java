/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.zones;

import org.springframework.context.annotation.Configuration;
import org.springframework.core.convert.converter.Converter;
import org.springframework.format.FormatterRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import com.vmware.blockchain.services.blockchains.zones.ZoneController.ZoneAction;

/**
 * Configuration so we can say action=reload in lowercase.
 */
@Configuration
public class ZoneConfig implements WebMvcConfigurer {
    static class ZoneActionConverter implements Converter<String, ZoneAction> {

        @Override
        public ZoneAction convert(String s) {
            return ZoneAction.valueOf(s.toUpperCase());
        }
    }

    @Override
    public void addFormatters(FormatterRegistry registry) {
        registry.addConverter(new ZoneActionConverter());
    }

}
