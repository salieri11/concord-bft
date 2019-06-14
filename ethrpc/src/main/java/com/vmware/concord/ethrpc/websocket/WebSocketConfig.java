/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.ethrpc.websocket;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.socket.config.annotation.EnableWebSocket;
import org.springframework.web.socket.config.annotation.WebSocketConfigurer;
import org.springframework.web.socket.config.annotation.WebSocketHandlerRegistry;

import com.vmware.concord.connections.ConcordConnectionPool;

/**
 * Temporary configuration to serve websocket.
 */
@Configuration
@EnableWebSocket
public class WebSocketConfig implements WebSocketConfigurer {
    private ConcordConnectionPool concordConnectionPool;

    @Autowired
    public WebSocketConfig(ConcordConnectionPool connectionPool) {
        concordConnectionPool = connectionPool;
    }

    public void registerWebSocketHandlers(WebSocketHandlerRegistry registry) {
        registry.addHandler(new Sockethandler(concordConnectionPool), "/ws").setAllowedOrigins("*");
    }

}
