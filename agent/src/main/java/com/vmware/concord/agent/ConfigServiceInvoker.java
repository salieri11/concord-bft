/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent;

import java.util.Collections;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.web.client.RestTemplate;

import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.kotlin.KotlinModule;
import com.vmware.blockchain.deployment.v1.ConfigurationComponent;
import com.vmware.blockchain.deployment.v1.ConfigurationServiceStub;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.NodeConfigurationRequest;
import com.vmware.blockchain.deployment.v1.NodeConfigurationResponse;
import com.vmware.blockchain.deployment.v1.TransportSecurity;

import io.grpc.CallOptions;
import io.grpc.ManagedChannelBuilder;

/**
 * Wrapper class which hides the gRPC vs REST invocation of the API.
 */
public class ConfigServiceInvoker {

    private static final Logger log = LoggerFactory.getLogger(ConfigServiceInvoker.class);

    private final Endpoint endpoint;
    private final boolean useRest;

    public ConfigServiceInvoker(Endpoint configServiceEndpoint, boolean useRest) {
        this.endpoint = configServiceEndpoint;
        this.useRest = useRest;
    }

    private static ConfigurationServiceStub generateConfigServiceStub(Endpoint configServiceEndpoint) {
        if (configServiceEndpoint.getTransportSecurity().getType()
            == TransportSecurity.Type.NONE) {
            return new ConfigurationServiceStub(
                    ManagedChannelBuilder
                            .forTarget(configServiceEndpoint.getAddress())
                            .usePlaintext()
                            .build(),
                    CallOptions.DEFAULT
            );
        } else {
            return new ConfigurationServiceStub(
                    ManagedChannelBuilder
                            .forTarget(configServiceEndpoint.getAddress())
                            .build(),
                    CallOptions.DEFAULT
            );
        }
    }

    /**
     * Retrieves the configuration for the node represented by this agent from Configuration service.
     *
     * @param session configuration session identifier.
     * @param node    node identifier.
     * @return list of {@link ConfigurationComponent}s.
     */
    public List<ConfigurationComponent> retrieveConfiguration(
            ConfigurationSessionIdentifier session,
            int node
    ) {
        var request = new NodeConfigurationRequest(new MessageHeader(), session, node);

        try {
            if (useRest) {
                ObjectMapper objectMapper = new ObjectMapper();
                objectMapper.registerModule(new KotlinModule());
                objectMapper.configure(MapperFeature.ACCEPT_CASE_INSENSITIVE_PROPERTIES, true);

                MappingJackson2HttpMessageConverter converter = new MappingJackson2HttpMessageConverter();
                converter.setObjectMapper(objectMapper);

                RestTemplate restTemplate = new RestTemplate();
                restTemplate.getMessageConverters().add(0, converter);

                final HttpHeaders headers = new HttpHeaders();
                headers.setContentType(MediaType.APPLICATION_JSON);

                ResponseEntity<NodeConfigurationResponse> result =
                        restTemplate.exchange(endpoint.getAddress(), HttpMethod.POST,
                                              new HttpEntity<>(request, headers),
                                              NodeConfigurationResponse.class);
                return result.getBody().getConfigurationComponent();

            } else {
                var responseObserver = new StreamObservers.MonoObserverFuture<NodeConfigurationResponse>();

                // Request for config.
                generateConfigServiceStub(endpoint).getNodeConfiguration(request, responseObserver);

                // Synchronously wait for result to return.
                return responseObserver.asCompletableFuture().join().getConfigurationComponent();
            }
        } catch (Exception e) {
            log.error("Configuration retrieval failed", e);
        }
        return Collections.emptyList();
    }
}
