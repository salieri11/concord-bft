/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

/**
 * Controller to serve up swagger document.
 */
@Controller
public class StaticContent {

    private static final long serialVersionUID = 1L;
    private static final Logger logger = LogManager.getLogger(StaticContent.class);

    // TODO: move these strings to properties file
    @RequestMapping(path = "/api")
    public String handleApiRequest() {
        return "swagger.json";
    }

}
