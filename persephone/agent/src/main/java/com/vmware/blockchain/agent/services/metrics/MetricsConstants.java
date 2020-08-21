/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.metrics;

/**
 * holds constants for metrics emission in agent.
 */
public class MetricsConstants {

    /**
     * metrics names constants.
     */
    public enum MetricsNames {
        WRITE("write_sec"),
        CREATE_NETWORK("containers.create_network"),
        CONTAINERS_PULL_IMAGES("containers.pull_images"),
        CONTAINER_PULL_IMAGE("container.pull_image"),
        CONTAINERS_LAUNCH("containers.launch_sec"),
        CONTAINER_LAUNCH("container.launch_sec"),
        CONTAINERS_LAUNCH_COUNT("containers.launch_counter"),
        CONTAINER_STOP("containers.stop_sec"),
        DAML_HEALTH_STATUS("daml.health"),
        CONCORD_HEALTH_STATUS("concord.health");

        String name;

        MetricsNames(String metricsName) {
            this.name = metricsName;
        }
    }

    /**
     * metrics tags constants.
     */
    public enum MetricsTags {
        TAG_METHOD("method"),
        TAG_SERVICE("service"),
        TAG_DOCKER_NETWORK("docker_network"),
        TAG_CONTAINER_ID("container_id"),
        TAG_NODE_TYPE("node_type"),
        TAG_COMPONENT("component"),
        TAG_IMAGE("image");

        String name;

        MetricsTags(String metricsTag) {
            this.name = metricsTag;
        }
    }
}
