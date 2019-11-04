/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import org.springframework.context.annotation.Configuration;
import org.springframework.session.jdbc.config.annotation.web.http.EnableJdbcHttpSession;


/**
 * Enable spring sessions.
 */
@EnableJdbcHttpSession
@Configuration
public class SessionConfig {
    //@EnableJdbcHttpSession handles everything with Java Magic (TM) so no need to use a session repo
}
