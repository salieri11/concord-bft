/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.base.base.feature;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Feature controller.
 */
@RestController
public class FeatureController {

    @RequestMapping("/")
    public String index() {
        return "Greetings from Spring Boot!";
    }
}
