/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.util;

import java.net.URI;

import org.junit.Assert;
import org.junit.Test;

import com.github.dockerjava.core.DefaultDockerClientConfig;
import com.vmware.blockchain.deployment.v1.Endpoint;


/**
 * Test for {@link DockerClientBuilderUtil}.
 */
public class DockerClientBuilderUtilTest {
    private String registryUsername = "username";
    private Endpoint registryEndpoint = Endpoint.newBuilder()
                                            .setAddress("http://container.image.registry.com")
                                            .build();
    private String registryPassword = "testPassword";

    @Test
    public void getDefaultDockerClient() {
        var clientConfig = DefaultDockerClientConfig.createDefaultConfigBuilder().build();

        var dockerClient = DockerClientBuilderUtil.createDockerClientWithClientConfig(clientConfig);
        Assert.assertNotNull(dockerClient);
    }

    @Test
    public void getDockerClientWithConfig() {
        var registryUri = URI.create(registryEndpoint.getAddress());
        var clientConfig = DefaultDockerClientConfig.createDefaultConfigBuilder()
                .withRegistryUrl(registryUri.getAuthority())
                .withRegistryUsername(registryUsername)
                .withRegistryPassword(registryPassword)
                .build();

        var dockerClient = DockerClientBuilderUtil.createDockerClientWithClientConfig(clientConfig);
        Assert.assertNotNull(dockerClient);
    }
}
