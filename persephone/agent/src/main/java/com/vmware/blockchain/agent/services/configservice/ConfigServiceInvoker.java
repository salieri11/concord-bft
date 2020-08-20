/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.configservice;

import java.util.Arrays;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.protobuf.ProtobufHttpMessageConverter;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.DefaultUriBuilderFactory;

import com.vmware.blockchain.agent.services.interceptor.retry.DefaultHttpRequestRetryInterceptor;
import com.vmware.blockchain.deployment.v1.ConfigurationComponent;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.NodeConfigurationRequest;
import com.vmware.blockchain.deployment.v1.NodeConfigurationResponse;

import lombok.extern.slf4j.Slf4j;

/**
 * Wrapper class which hides the gRPC vs REST invocation of the API.
 */
@Slf4j
@Component
public class ConfigServiceInvoker {

    private final Endpoint endpoint;
    private final RestTemplate restTemplate;

    @Autowired
    public ConfigServiceInvoker(Endpoint configServiceEndpoint) {
        this.endpoint = configServiceEndpoint;
        restTemplate = new RestTemplate(Arrays.asList(new ProtobufHttpMessageConverter()));
        restTemplate.getInterceptors().add(DefaultHttpRequestRetryInterceptor.getDefaultInstance());
        restTemplate.setUriTemplateHandler(new DefaultUriBuilderFactory(endpoint.getAddress()));
    }

    /**
     * Retrieves the configuration for the node represented by this agent from Configuration service.
     *
     * @param session configuration session identifier.
     * @param nodeId    node identifier.
     * @return list of {@link ConfigurationComponent}s.
     */
    public List<ConfigurationComponent> retrieveConfiguration(ConfigurationSessionIdentifier session, String nodeId) {
        var request = NodeConfigurationRequest.newBuilder().setHeader(MessageHeader.newBuilder().build())
                .setIdentifier(session);

        request.setNodeId(nodeId);
        try {

            final HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);

            HttpEntity<NodeConfigurationRequest> entity = new HttpEntity<>(request.build(), headers);

            ResponseEntity<NodeConfigurationResponse> result =
                    restTemplate.exchange("/v1/configuration/node", HttpMethod.POST,
                                          entity,
                                          NodeConfigurationResponse.class);

            return result.getBody().getConfigurationComponentList();
        } catch (Exception e) {
            log.error("Configuration retrieval failed", e);
            throw e;
        }
    }
}