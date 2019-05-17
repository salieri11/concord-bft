/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent;

import static com.github.dockerjava.api.model.HostConfig.newHostConfig;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Properties;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.amazonaws.services.s3.model.S3Object;
import com.amazonaws.services.s3.model.S3ObjectInputStream;

import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.api.command.CreateContainerResponse;
import com.github.dockerjava.api.model.Bind;
import com.github.dockerjava.api.model.ExposedPort;
import com.github.dockerjava.api.model.Image;
import com.github.dockerjava.api.model.Link;
import com.github.dockerjava.api.model.PortBinding;
import com.github.dockerjava.api.model.Ports;
import com.github.dockerjava.core.DockerClientBuilder;

import com.vmware.blockchain.awsutil.AwsS3Client;

/**
 * Utility class for talking to Docker and creating the required volumes, starting
 * concord-node etc.
 */
public final class AgentDockerClient {
    private static final Logger log = LoggerFactory.getLogger(AgentDockerClient.class);

    private static final String CONCORD_DOCKER = "registry-1.docker.io/vmwblockchain/concord-core:latest";
    private static final String ETHRPC_DOCKER = "registry-1.docker.io/vmwblockchain/ethrpc:latest";
    private static final String DEFAULT_VOLUME = "/concord/config-local";


    private String concordDockerImage;
    private String ethrpcDockerImage;
    private String dockerVolumeName;
    private String concordImageId;
    private String ethrpcImageId;
    private DockerClient dockerClient;

    /** Default application.properties file. */
    private String propertyFile = "applications.properties";

    /** S3 Client. */
    private AwsS3Client awsClient;

    /** S3 bucket. */
    private String s3bucket;

    private boolean initializeS3Client() {
        log.info("Reading the application properties file");
        try (InputStream input = getClass().getClassLoader().getResourceAsStream(propertyFile)) {
            // TODO - these default values needs to be specific to CICD pipeline account.
            String awsKey = "AKIATWKQRJCJPZPBH4PK";
            String secretKey = "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";
            String region = "us-west-2";
            s3bucket = "concord-config";

            Properties prop = new Properties();
            if (input != null) {
                prop.load(input);
                awsKey = prop.getProperty("aws.accessKey");
                secretKey = prop.getProperty("aws.secretKey");
                region = prop.getProperty("aws.region");
                s3bucket = prop.getProperty("aws.bucket");
            } else {
                log.error("Unable to find property file: " + propertyFile);
            }
            awsClient = new AwsS3Client(awsKey, secretKey, region);
            if (awsClient == null) {
                log.error("Unable to create AWS client: " + propertyFile);
                return false;
            }
            log.info("Create s3 client successfully");
        } catch (IOException ex) {
            log.error("Unable to read the property file: " + propertyFile);
            return false;
        }
        return true;
    }

    /**
     * Default constructor.
     */
    public AgentDockerClient() {
        concordDockerImage = CONCORD_DOCKER;
        ethrpcDockerImage = ETHRPC_DOCKER;
        dockerVolumeName = DEFAULT_VOLUME;
        initializeS3Client();
    }

    /**
     * Constructor with specific names.
     */
    public AgentDockerClient(String concordImage, String ethrpcImage, String volume) {
        concordDockerImage = concordImage;
        ethrpcDockerImage = ethrpcImage;
        dockerVolumeName = volume;
        initializeS3Client();
    }

    private void populateConfig() {
        try {
            String clusterId = System.getenv("CID");
            String nodeId = System.getenv("NID");
            String configPath = clusterId + "/" + nodeId;
            S3Object s3object = awsClient.getObject(s3bucket, configPath);
            S3ObjectInputStream inputStream = s3object.getObjectContent();
            FileUtils.copyInputStreamToFile(inputStream, new File("/concord/config-local/concord.config"));

            log.info("Copied the " + configPath + "to concord.config");
            File src = new File("/concord/config-local/concord.config");
            File copied = new File("/concord/config-local/concord_with_hostnames.config");
            FileUtils.copyFile(src, copied);
        } catch (IOException ex) {
            log.error("Couldn't write to s3 bucket");
        }
    }

    /**
     * Create the docker volume in the root node.
     */
    public void startConcord() {
        log.info("Reading config file");
        populateConfig();
        log.info("Populated the config file");

        dockerClient = DockerClientBuilder.getInstance().build();
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

        if (concordDockerImage != null) {
            CreateContainerResponse container = dockerClient.createContainerCmd("concord-core")
                       .withName("concord")
                       .withImage(concordImageId)
                       .withHostConfig(newHostConfig()
                            .withPortBindings(new PortBinding(Ports.Binding.bindPort(5458), ExposedPort.tcp(5458)),
                                              new PortBinding(Ports.Binding.bindPort(3501), ExposedPort.udp(3501)),
                                              new PortBinding(Ports.Binding.bindPort(3502), ExposedPort.udp(3502)),
                                              new PortBinding(Ports.Binding.bindPort(3503), ExposedPort.udp(3503)),
                                              new PortBinding(Ports.Binding.bindPort(3504), ExposedPort.udp(3504)),
                                              new PortBinding(Ports.Binding.bindPort(3505), ExposedPort.udp(3505)))
                            .withBinds(Bind.parse("/concord/config-local:/concord/config-local"),
                                       Bind.parse("/concord/config-public:/concord/config-public")))
                       .exec();

            // Now lets start the docker image
            if (container == null) {
                log.error("Couldn't start the container...!");
            } else {
                log.info("Starting container: " + container.getId());
                dockerClient.startContainerCmd(container.getId()).exec();
                log.info("started concord: " + container.getId());
            }
        }
        // TODO: this should be done in Dockerfile and not as argument while starting the daemon.
        String cmd = "sed -i s/localhost/concord/g application.properties && java -jar concord-ethrpc.jar";
        if (ethrpcImageId != null) {
            CreateContainerResponse container = dockerClient.createContainerCmd("ethrpc")
                  .withName("ethrpc")
                  .withImage(ethrpcImageId)
                  .withCmd("/bin/bash", "-c", cmd)
                  .withHostConfig(newHostConfig()
                       .withPortBindings(new PortBinding(Ports.Binding.bindPort(8545), ExposedPort.tcp(8545)))
                       .withLinks(new Link("concord", "concord"))).exec();

            // Now lets start the docker image
            if (container == null) {
                log.error("Couldn't start ethrpc container...!");
            } else {
                log.info("Starting ethrpc: " + container.getId());
                dockerClient.startContainerCmd(container.getId()).exec();
                log.info("Started container: " + container.getId());
            }
        }
    }
}
