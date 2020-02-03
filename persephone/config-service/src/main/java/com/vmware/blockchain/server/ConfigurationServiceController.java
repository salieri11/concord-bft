/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server;

import java.util.concurrent.CompletableFuture;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.vmware.blockchain.deployment.v1.NodeConfigurationRequest;
import com.vmware.blockchain.deployment.v1.NodeConfigurationResponse;

import io.grpc.stub.StreamObserver;
import lombok.extern.slf4j.Slf4j;

/**
 * Implementation of ConfigurationService server.
 */
@RestController
@Slf4j
public class ConfigurationServiceController {

    @Autowired
    ConfigurationService configurationService;

    /**
     * Rest.
     */
    @PostMapping(path = "/v1/configuration/node")
    public ResponseEntity<NodeConfigurationResponse> getNodeConfiguration(@RequestBody NodeConfigurationRequest
                                                                                  request) throws Exception {
        log.info("Received request: " + request);
        var promise = new CompletableFuture<NodeConfigurationResponse>();
        configurationService.getNodeConfiguration(request, newResultObserver(promise));
        return new ResponseEntity<>(promise.get(), HttpStatus.OK);
    }

    private static <T> StreamObserver<T> newResultObserver(CompletableFuture<T> result) {
        return new StreamObserver<>() {
            /** Holder of result value. */
            volatile T value;

            @Override
            public void onNext(T value) {
                this.value = value;
            }

            @Override
            public void onError(Throwable error) {
                result.completeExceptionally(error);
            }

            @Override
            public void onCompleted() {
                result.complete(value);
            }
        };
    }
}