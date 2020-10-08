/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.server;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

import com.vmware.blockchain.deployment.v1.Credential;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.PasswordCredential;
import com.vmware.blockchain.deployment.v1.TransportSecurity;

/**
 * Bootstrap application properties.
 */
@Configuration
public class BootstrapComponent {

    @Value("${provisioning.config.service.address}")
    private String csEndpoint;

    @Value("${provisioning.config.service.transportSecurity.type}")
    private String csEndpointSecurity = "None";

    @Value("${provisioning.config.service.rest.address}")
    private String csEndpointRest = "";

    @Value("${provisioning.allocation.service.address}")
    private String asEndpoint;

    @Value("${provisioning.allocation.service.transportSecurity.type}")
    private String asEndpointSecurity = "NONE";

    @Value("${provisioning.container.registry.address}")
    private String crEndpoint;

    @Value("${provisioning.container.registry.username}")
    private String username;

    @Value("${provisioning.container.registry.password}")
    private String password;

    @Value("${ova.template:8abc7fda-9576-4b13-9beb-06f867cf2c7c}")
    public String template;

    @Value("${replica.wait:300000}")
    public int waitForReplica;

    @Value("${provisioning.certs:/certs}")
    public String pathToCerts;

    public Endpoint configService;

    public Endpoint configServiceRest;

    public Endpoint allocationService;

    public Endpoint containerRegistry;

    /**
     * Configuration input.
     */
    @PostConstruct
    public void initialize() {
        configService = Endpoint.newBuilder()
                .setTransportSecurity(
                        TransportSecurity.newBuilder().setType(TransportSecurity.Type.valueOf(csEndpointSecurity))
                                .build())
                .setAddress(csEndpoint).build();

        configServiceRest = Endpoint.newBuilder()
                .setAddress(csEndpointRest).build();

        allocationService = Endpoint.newBuilder()
                .setTransportSecurity(
                        TransportSecurity.newBuilder().setType(TransportSecurity.Type.valueOf(asEndpointSecurity))
                                .build())
                .setAddress(asEndpoint).build();

        containerRegistry = Endpoint.newBuilder()
                .setAddress(crEndpoint)
                .setCredential(Credential.newBuilder()
                                       .setType(Credential.Type.PASSWORD)
                                       .setPasswordCredential(PasswordCredential.newBuilder()
                                                                      .setUsername(username)
                                                                      .setPassword(password)
                                                                      .build()).build())

                .build();
    }
}
