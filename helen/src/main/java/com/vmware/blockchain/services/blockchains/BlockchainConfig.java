/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import org.springframework.context.annotation.Configuration;
import org.springframework.core.convert.converter.Converter;
import org.springframework.format.FormatterRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import com.vmware.blockchain.services.blockchains.BlockchainReplicaController.NodeAction;

/**
 * Configure a converter for blockchain params.  This is so you can say "?action=stop" instead of
 * "?action=STOP"
 */
@Configuration
public class BlockchainConfig implements WebMvcConfigurer {
    static class NodeActionConverter implements Converter<String, NodeAction> {

        @Override
        public NodeAction convert(String s) {
            return NodeAction.valueOf(s.toUpperCase());
        }
    }

    @Override
    public void addFormatters(FormatterRegistry registry) {
        registry.addConverter(new NodeActionConverter());
    }
}
