/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.util;

import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.core.DefaultDockerClientConfig;
import com.github.dockerjava.core.DockerClientBuilder;
import com.github.dockerjava.jaxrs.JerseyDockerHttpClient;

/**
 * Utility class for Docker Client related methods.
 */
public class DockerClientBuilderUtil {
    /**
     * Create a docker client using the provided client config.
     * @param clientConfig Client config containing Registry details
     * @return dockerClient
     */
    public static DockerClient createDockerClientWithClientConfig(DefaultDockerClientConfig clientConfig) {
        var dockerHttpClientBuilder = new JerseyDockerHttpClient.Builder();

        if (clientConfig.getDockerHost() != null) {
            dockerHttpClientBuilder.dockerHost(clientConfig.getDockerHost());
        }

        if (clientConfig.getSSLConfig() != null) {
            dockerHttpClientBuilder.sslConfig(clientConfig.getSSLConfig());
        }

        return DockerClientBuilder.getInstance(clientConfig)
                .withDockerHttpClient(dockerHttpClientBuilder.build())
                .build();
    }

    /**
     * Create a default docker client.
     * @return dockerClient
     */
    public static DockerClient createDefaultDockerClient() {
        var clientConfig = DefaultDockerClientConfig.createDefaultConfigBuilder().build();

        JerseyDockerHttpClient dockerHttpClient = new JerseyDockerHttpClient.Builder()
                .dockerHost(clientConfig.getDockerHost())
                .sslConfig(clientConfig.getSSLConfig())
                .build();

        return DockerClientBuilder.getInstance()
                .withDockerHttpClient(dockerHttpClient)
                .build();
    }
}
