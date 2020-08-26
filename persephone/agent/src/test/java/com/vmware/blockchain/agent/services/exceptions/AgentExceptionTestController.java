/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.exceptions;

import java.util.UUID;

import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;


@RestController
public class AgentExceptionTestController {

    @RequestMapping(path = "/api/gg/{sessionid}", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
    ResponseEntity<Void> getNewConfiguration(@PathVariable("sessionid") UUID sessionid) {
        var rte = new RuntimeException("Some");
         throw new AgentException(ErrorCode.CONFIGURATION_RETRIEVAL_FAILURE, rte.getMessage(), rte);
    }

}
