/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent.services.configservice;

import java.util.List;

import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.web.client.RestTemplate;

import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.vmware.blockchain.deployment.v1.ConfigurationComponent;
import com.vmware.blockchain.deployment.v1.ConfigurationServiceGrpc;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.NodeConfigurationRequest;
import com.vmware.blockchain.deployment.v1.NodeConfigurationResponse;
import com.vmware.blockchain.deployment.v1.TransportSecurity;

import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import lombok.extern.slf4j.Slf4j;

/**
 * Wrapper class which hides the gRPC vs REST invocation of the API.
 */
@Slf4j
public class ConfigServiceInvoker {

    private final Endpoint endpoint;
    private final boolean useRest;

    public ConfigServiceInvoker(Endpoint configServiceEndpoint, boolean useRest) {
        this.endpoint = configServiceEndpoint;
        this.useRest = useRest;
    }

    private static ConfigurationServiceGrpc.ConfigurationServiceFutureStub generateConfigServiceStub(
            Endpoint configServiceEndpoint) {
        ManagedChannel channel = null;
        if (configServiceEndpoint.getTransportSecurity().getType()
            == TransportSecurity.Type.NONE) {
            channel = ManagedChannelBuilder
                    .forTarget(configServiceEndpoint.getAddress())
                    .usePlaintext()
                    .build();
        } else {
            channel = ManagedChannelBuilder
                    .forTarget(configServiceEndpoint.getAddress())
                    .build();
        }
        return ConfigurationServiceGrpc.newFutureStub(channel);
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
    ) throws Exception {
        var request = NodeConfigurationRequest.newBuilder().setHeader(MessageHeader.newBuilder().build())
                .setIdentifier(session).setNode(node).build();

        try {
            if (useRest) {
                ObjectMapper objectMapper = new ObjectMapper();
                objectMapper.configure(MapperFeature.ACCEPT_CASE_INSENSITIVE_PROPERTIES, true);

                MappingJackson2HttpMessageConverter converter = new MappingJackson2HttpMessageConverter();
                converter.setObjectMapper(objectMapper);

                RestTemplate restTemplate = new RestTemplate();
                restTemplate.getMessageConverters().add(0, converter);

                final HttpHeaders headers = new HttpHeaders();
                headers.setContentType(MediaType.APPLICATION_JSON);

                ResponseEntity<NodeConfigurationResponse> result =
                        restTemplate.exchange(endpoint.getAddress() + "/v1/configuration/node", HttpMethod.POST,
                                              new HttpEntity<>(request, headers),
                                              NodeConfigurationResponse.class);
                return result.getBody().getConfigurationComponentList();

            } else {
                var promise = generateConfigServiceStub(endpoint).getNodeConfiguration(request);

                // Synchronously wait for result to return.
                return promise.get().getConfigurationComponentList();
            }
        } catch (Exception e) {
            log.error("Configuration retrieval failed", e);
            throw e;
        }
    }
}
