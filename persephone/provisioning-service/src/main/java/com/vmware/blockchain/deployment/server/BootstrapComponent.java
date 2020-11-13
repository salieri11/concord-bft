/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.server;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

import com.vmware.blockchain.deployment.services.exception.BootstrapProvisioningServiceException;
import com.vmware.blockchain.deployment.v1.Credential;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.PasswordCredential;
import com.vmware.blockchain.deployment.v1.TransportSecurity;

import lombok.extern.slf4j.Slf4j;

/**
 * Bootstrap application properties.
 */
@Configuration
@Slf4j
public class BootstrapComponent {
    @Value("${provisioning.config.service.address}")
    private String csEndpoint;

    @Value("${provisioning.config.service.transportSecurity.type:NONE}")
    private String csEndpointSecurity;

    @Value("${provisioning.config.service.rest.address}")
    private String csEndpointRest = "";

    @Value("${provisioning.allocation.service.address}")
    private String asEndpoint;

    @Value("${provisioning.allocation.service.transportSecurity.type:NONE}")
    private String asEndpointSecurity;

    @Value("${provisioning.container.registry.address}")
    private String crEndpoint;

    @Value("${provisioning.container.registry.username}")
    private String username;

    @Value("${provisioning.container.registry.password:NONE}")
    private String password;

    @Value("${provisioning.notary.server.address:https://notary.vdp.vmware.com}")
    private String notaryServerAddress;

    @Value("${ova.template:28b41974-c7a8-41a7-84f8-4438f828e87b}")
    public String template;

    @Value("${replica.wait:300000}")
    public int waitForReplica;

    @Value("${provisioning.certs:/config/app/certs}")
    public String pathToCerts;

    public Endpoint configService;

    public Endpoint configServiceRest;

    public Endpoint allocationService;

    public Endpoint containerRegistry;

    public Endpoint notaryServer;

    /**
     * Configuration input.
     */
    @PostConstruct
    public void initialize() {
        TransportSecurity.Type csSecurityType = getTransportSecurityType(csEndpointSecurity);
        configService = Endpoint.newBuilder()
                .setTransportSecurity(
                        TransportSecurity.newBuilder().setType(csSecurityType)
                                .build())
                .setAddress(csEndpoint).build();

        configServiceRest = Endpoint.newBuilder()
                .setAddress(csEndpointRest).build();

        TransportSecurity.Type asSecurityType = getTransportSecurityType(asEndpointSecurity);
        allocationService = Endpoint.newBuilder()
                .setTransportSecurity(
                        TransportSecurity.newBuilder().setType(asSecurityType)
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

        notaryServer = Endpoint.newBuilder().setAddress(notaryServerAddress).build();
        if (!validateBootstrapConfiguration(configService, allocationService)) {
            throw new BootstrapProvisioningServiceException("Invalid Application configuration");
        }
    }

    private TransportSecurity.Type getTransportSecurityType(String typeStr)
            throws BootstrapProvisioningServiceException {
        if (typeStr == null || typeStr.isEmpty()) {
            log.error("Invalid transport type {}", typeStr);
            throw new BootstrapProvisioningServiceException("Invalid Configuration Service transport type in "
                                                            + "application.properties file.");
        }
        TransportSecurity.Type configType = null;
        TransportSecurity.Type[] types = TransportSecurity.Type.values();
        for (TransportSecurity.Type type : types) {
            if (typeStr.equalsIgnoreCase(type.name())) {
                configType = type;
                break;
            }
        }
        if (configType == null) {
            throw new BootstrapProvisioningServiceException("Invalid Transport security type " + typeStr);
        }
        return configType;
    }

    /**
     * Validate bootstrap configuration in applicaiton.properties file.
     * @param configService confguration service
     * @param ipamService IPAM service
     * @return True if configuration is valid, false otherwise.
     */
    private boolean validateBootstrapConfiguration(Endpoint configService, Endpoint ipamService) {
        if (configService == null || configService.getTransportSecurity() == null
            || configService.getTransportSecurity().getType() == null) {
            log.error("Configuration service settings are incorrect.");
            return false;
        }
        if (ipamService == null || ipamService.getTransportSecurity() == null
            || ipamService.getTransportSecurity().getType() == null) {
            log.error("IPAM service settings are incorrect.");
            return false;
        }
        /**
         * Validate the environment settings
         * provisioning.config.service.address=config-service:9003
         *       - provisioning.config.service.transportSecurity.type=NONE
         *       - provisioning.config.service.rest.address=http:10.1.2.3:8000
         *       - provisioning.allocation.service.address=ipam-vmbc.cloud.vmware.com
         *       - provisioning.allocation.service.transportSecurity.type=TLSv1_2
         */
        // Do we need to talk in TLS to any service?
        if (configService.getTransportSecurity().getType() == TransportSecurity.Type.TLSv1_2
            || ipamService.getTransportSecurity().getType() == TransportSecurity.Type.TLSv1_2) {

            if (pathToCerts == null || pathToCerts.isEmpty()) {
                log.error("pathToCerts is a required field when TLS is enabled.");
                return false;
            }

            // Is the path available?
            Path certsDir = Path.of(pathToCerts);
            try {
                // Return false if the certsDir does not exist, or if it is not a directory or it is empty.
                if (!Files.exists(certsDir, LinkOption.NOFOLLOW_LINKS) || !Files.isDirectory(certsDir) || !Files
                        .list(certsDir).findFirst().isPresent()) {
                    return false;
                }
            } catch (IOException ioe) {
                log.error("Trouble with the file system.", ioe);
                return false;
            }
        }
        return true;
    }

}
