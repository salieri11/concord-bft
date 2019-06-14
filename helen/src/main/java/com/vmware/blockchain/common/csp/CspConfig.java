/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp;

import java.net.URISyntaxException;

import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.utils.URIBuilder;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * Configuration properties for csp.
 */
@Component
public class CspConfig {
    private static Logger logger = LogManager.getLogger(CspConfig.class);

    @Value("${csp.port:#{null}}")
    private String cspPort;

    @Value("${csp.host:#{null}}")
    private String cspHost;

    @Value("${csp.protocol:#{null}}")
    private String cspProtocol;

    @Value("${csp.url:#{null}}")
    private String cspUrl;

    @Value("${vmbc.csp.service.user:#{null}}")
    private String cspUserName;

    @Value("${vmbc.csp.service.user.password:#{null}}")
    private String cspPassword;

    /**
     * Generate a proper URL for CSP based on the following application.properties entries
     * csp.host=${HOST:#{null}}
     * csp.port=${CSP_PORT_0:#{null}}
     * csp.protocol=${CSP_PROTOCOL:http}
     *
     * @return Correct url for accessing CSP
     */
    public String getCspUrl() {
        if (!StringUtils.isEmpty(cspUrl)) {
            return cspUrl;
        } else if (!StringUtils.isEmpty(cspHost) && !StringUtils.isEmpty(cspPort)) {
            URIBuilder cspUri = new URIBuilder();
            cspUri.setScheme(cspProtocol).setHost(cspHost).setPort(Integer.parseInt(cspPort));
            try {
                return cspUri.build().toString();
            } catch (URISyntaxException e) {
                logger.error("Malformed CSP URL", e);
                return "";
            }
        } else {
            return "";
        }
    }

    public String getCspUserName() {
        return cspUserName;
    }

    public String getCspPassword() {
        return cspPassword;
    }
}
