/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent.services.configservice;

import java.util.Arrays;
import java.util.List;

import org.assertj.core.util.Strings;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.protobuf.ProtobufHttpMessageConverter;
import org.springframework.web.client.RestTemplate;

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
public class ConfigServiceInvoker {

    private final Endpoint endpoint;

    public ConfigServiceInvoker(Endpoint configServiceEndpoint) {
        this.endpoint = configServiceEndpoint;
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
            int node,
            String nodeId
    ) {
        var request = NodeConfigurationRequest.newBuilder().setHeader(MessageHeader.newBuilder().build())
                .setIdentifier(session);

        if (Strings.isNullOrEmpty(nodeId)) {
            request.setNode(node);
        } else {
            request.setNodeId(nodeId);
        }
        try {
            final RestTemplate restTemplate = new RestTemplate(Arrays.asList(new ProtobufHttpMessageConverter()));

            final HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);

            HttpEntity<NodeConfigurationRequest> entity = new HttpEntity<>(request.build(), headers);

            ResponseEntity<NodeConfigurationResponse> result =
                    restTemplate.exchange(endpoint.getAddress() + "/v1/configuration/node", HttpMethod.POST,
                            entity,
                            NodeConfigurationResponse.class);

            return result.getBody().getConfigurationComponentList();
        } catch (Exception e) {
            log.error("Configuration retrieval failed", e);
            throw e;
        }
    }
}
