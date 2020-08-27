/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.exceptions;

import java.util.UUID;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

/**
 * Test controller, used to test Agent Exception Handler independently.
 * */
@RestController
public class AgentExceptionTestController {

    @RequestMapping(path = "/api/config-error/{sessionid}", method = RequestMethod.POST,
                                                            produces = MediaType.APPLICATION_JSON_VALUE)
    ResponseEntity<Void> getErrorConfiguration(@PathVariable("sessionid") UUID sessionid) {
        var rte = new RuntimeException("Some");
        throw new AgentException(ErrorCode.CONFIGURATION_RETRIEVAL_FAILURE, rte.getMessage(), rte);
    }

    @RequestMapping(path = "/api/random", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
    ResponseEntity<Void> getRandomException() {
        throw new RuntimeException("Some");
    }

    @RequestMapping(path = "/api/ok", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
    ResponseEntity<Void> noException() {
        return new ResponseEntity<>(HttpStatus.OK);
    }

}
