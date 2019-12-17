/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.operation;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.Assertions;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.vmware.blockchain.common.BadRequestException;

/**
 * Test controller for operation context.
 */
@RestController
public class OperationTestController {
    private static Logger logger = LogManager.getLogger(OperationTestController.class);

    @Autowired
    OperationContext operationContext;

    @RequestMapping(method = RequestMethod.GET, path = "/api/authtest")
    void apiAuthTest() {
        logger.info("In the controller method");
        Assertions.assertNotNull(operationContext.getId());
    }

    @RequestMapping(method = RequestMethod.GET, path = "/api/bad")
    void apiBadTest() {
        logger.info("In the controller method");
        throw new BadRequestException("Bad request");
    }
}
