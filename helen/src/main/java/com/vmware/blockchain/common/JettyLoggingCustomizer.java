/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.jetty.server.Handler;
import org.eclipse.jetty.server.Request;
import org.eclipse.jetty.server.RequestLog;
import org.eclipse.jetty.server.Response;
import org.eclipse.jetty.server.handler.HandlerCollection;
import org.eclipse.jetty.server.handler.RequestLogHandler;
import org.springframework.boot.web.embedded.jetty.JettyServletWebServerFactory;
import org.springframework.boot.web.server.WebServerFactoryCustomizer;
import org.springframework.stereotype.Component;

/**
 * Web servlet customization to add http request logging.
 */
@Component
public class JettyLoggingCustomizer implements WebServerFactoryCustomizer<JettyServletWebServerFactory> {
    private static Logger logger = LogManager.getLogger();

    static class RequestLogImpl implements RequestLog {
        @Override
        public void log(Request request, Response response) {
            request.getMethod();
            logger.info("{} {} {} {} {}", request.getRemoteHost(), request.getMethod(),
                        request.getRequestURI(), response.getStatus(), response.getContentCount());
        }
    }

    /**
     * Customize Jetty with logging.
     */
    @Override
    public void customize(JettyServletWebServerFactory factory) {
        factory.addServerCustomizers(server -> {
            HandlerCollection handlers = new HandlerCollection();
            for (Handler handler : server.getHandlers()) {
                handlers.addHandler(handler);
            }
            RequestLogHandler reqLogs = new RequestLogHandler();
            RequestLogImpl reqLogImpl = new RequestLogImpl();
            reqLogs.setRequestLog(reqLogImpl);
            handlers.addHandler(reqLogs);
            server.setHandler(handlers);
        });
    }
}
