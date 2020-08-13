/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.configservice;

import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;

import lombok.extern.slf4j.Slf4j;

/**
 * Controller to pull config explicitly.
 */
@RestController
@Slf4j
public class ConfigController {

    private ConfigServiceInvoker configServiceInvoker;
    private String nodeId;

    @Autowired
    public ConfigController(ConfigServiceInvoker configServiceInvoker, String nodeId) {
        this.configServiceInvoker = configServiceInvoker;
        this.nodeId = nodeId;
    }

    /**
     * Get the list of all participant nodes.
     */
    @RequestMapping(path = "/api/reconfigure/{sessionid}", method = RequestMethod.POST)
    ResponseEntity<Void> getNewConfiguration(@PathVariable("sessionid") UUID sessionid) {
        log.info("Received request to download config with identifier {}", sessionid);
        // TODO Add error handling.
        configServiceInvoker
                .retrieveConfiguration(ConfigurationSessionIdentifier.newBuilder().setId(sessionid.toString()).build(),
                                       nodeId);
        return new ResponseEntity<>(HttpStatus.OK);
    }

}