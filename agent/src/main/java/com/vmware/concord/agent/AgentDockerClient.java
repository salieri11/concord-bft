/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent;

import static com.github.dockerjava.api.model.HostConfig.newHostConfig;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.api.command.CreateContainerResponse;
import com.github.dockerjava.api.command.CreateVolumeResponse;
import com.github.dockerjava.api.model.Bind;
import com.github.dockerjava.api.model.Image;
import com.github.dockerjava.core.DockerClientBuilder;



/**
 * Utility class for talking to Docker and creating the required volumes, starting
 * concord-node etc.
 */
public final class AgentDockerClient {

    private static final Logger log = LoggerFactory.getLogger(AgentDockerClient.class);

    private static final String CONCORD_DOCKER = "registry-1.docker.io/vmwblockchain/concord-core:latest";
    private static final String ETHRPC_DOCKER = "registry-1.docker.io/vmwblockchain/ethrpc:latest";
    private static final String DEFAULT_VOLUME = "vmwblockchain";

    private String concordDockerImage;
    private String ethrpcDockerImage;
    private String dockerVolumeName;
    private String concordImageId;
    private String ethrpcImageId;
    private DockerClient dockerClient;

    /**
     * Default constructor.
     */
    public AgentDockerClient() {
        concordDockerImage = CONCORD_DOCKER;
        ethrpcDockerImage = ETHRPC_DOCKER;
        dockerVolumeName = DEFAULT_VOLUME;
    }

    /**
     * Constructor with specific names.
     */
    public AgentDockerClient(String concordImage, String ethrpcImage, String volume) {
        concordDockerImage = concordImage;
        ethrpcDockerImage = ethrpcImage;
        dockerVolumeName = volume;
    }

    /**
     * Create the docker volume in the root node.
     */
    public void createVolume() {
        dockerClient = DockerClientBuilder.getInstance().build();
        log.info("After first connection");
        CreateVolumeResponse conVol = dockerClient.createVolumeCmd().withName(dockerVolumeName).exec();

        List<Image> images = dockerClient.listImagesCmd().exec();
        for (Image image : images) {
            log.info("image: id: " + image.getId());
            log.info("image: repoTags: ");
            if (image.getRepoTags() != null) {
                for (String name : image.getRepoTags()) {
                    if (name.equals(concordDockerImage)) {
                        concordImageId = image.getId();
                        log.info("           image: " + name);
                    } else if (name.equals(ethrpcDockerImage)) {
                        ethrpcImageId = image.getId();
                        log.info("           image: " + name);
                    }
                }
            }
        }
        // Now lets start the concord-core with the config files mounted.
        CreateContainerResponse container = dockerClient.createContainerCmd("concord-core:latest")
                                                 .withImage(concordImageId)
                                                 .withHostConfig(newHostConfig()
                                                      .withBinds(Bind.parse("/config:/config"))).exec();
        // Now lets start the docker image
        if (container == null) {
            log.error("Couldn't start the container...!");
        } else {
            log.info("Starting container: " + container.getId());
            dockerClient.startContainerCmd(container.getId()).exec();
        }
    }
}
