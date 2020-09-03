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
        WRITE("agent.write_sec"),
        CREATE_NETWORK("agent.containers.create_network"),
        CONTAINERS_PULL_IMAGES("agent.containers.pull_images"),
        CONTAINER_PULL_IMAGE("agent.container.pull_image"),
        CONTAINERS_LAUNCH("agent.containers.launch_sec"),
        CONTAINER_LAUNCH("agent.container.launch_sec"),
        CONTAINERS_LAUNCH_COUNT("agent.containers.launch_counter"),
        CONTAINER_STOP("agent.containers.stop_sec"),
        DAML_HEALTH_STATUS("agent.daml.health"),
        CONCORD_HEALTH_STATUS("agent.concord.health");

        public final String metricsName;

        MetricsNames(String metricsName) {
            this.metricsName = metricsName;
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
        TAG_CONTAINER_NAME("container_name"),
        TAG_NODE_TYPE("node_type"),
        TAG_COMPONENT("component"),
        TAG_IMAGE("image");

        public final String metricsTagName;

        MetricsTags(String metricsTagName) {
            this.metricsTagName = metricsTagName;
        }
    }
}
