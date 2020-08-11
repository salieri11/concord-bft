/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.configuration;

import java.util.List;

import com.github.dockerjava.api.model.Bind;
import com.github.dockerjava.api.model.ExposedPort;
import com.github.dockerjava.api.model.Link;
import com.github.dockerjava.api.model.LogConfig;
import com.github.dockerjava.api.model.PortBinding;
import com.github.dockerjava.api.model.Ports;
import com.github.dockerjava.api.model.RestartPolicy;

import lombok.Getter;
import lombok.Setter;

/**
 * Enumeration for the metrics and tracing container configurations.
 */
@Getter
public enum MetricsAndTracingConfig implements BaseContainerSpec {

    LOGGING(LogConfig.LoggingType.FLUENTD.toString(), null,
            List.of(Bind.parse("/var/lib/docker/containers:/var/lib/docker/containers")),
            null,  null, null),
    WAVEFRONT_PROXY("wavefront-proxy",
            List.of(new PortBinding(Ports.Binding.bindPort(14267), ExposedPort.tcp(14267))),
            null, null, null,
            List.of("WAVEFRONT_PROXY_ARGS=-f /config/wavefront-proxy/wavefront.conf -m 2g",
                    "JAVA_HEAP_USAGE=\"1650m\"")),
    JAEGER_AGENT("jaeger-agent",
            List.of(new PortBinding(Ports.Binding.bindPort(5775), ExposedPort.udp(5775)),
                    new PortBinding(Ports.Binding.bindPort(6831), ExposedPort.udp(6831)),
                    new PortBinding(Ports.Binding.bindPort(6832), ExposedPort.udp(6832)),
                    new PortBinding(Ports.Binding.bindPort(5778), ExposedPort.tcp(5778))),
            null,
            List.of(new Link("wavefront-proxy", "wavefront-proxy")),
            null,
            List.of("REPORTER_TCHANNEL_HOST_PORT=wavefront-proxy:14267",
                    "REPORTER_TYPE=tchannel")),
    TELEGRAF("telegraf",
            List.of(new PortBinding(Ports.Binding.bindPort(9090), ExposedPort.tcp(9090))),
            List.of(Bind.parse("/config/telegraf/telegraf.conf:/etc/telegraf/telegraf.conf"),
                    Bind.parse("/var/run/docker.sock:/var/run/docker.sock:ro"),
                    Bind.parse("/sys:/hostfs/sys:ro"),
                    Bind.parse("/proc:/hostfs/proc:ro")),
            null, null,
            List.of("HOST_MOUNT_PREFIX=/hostfs",
                    "HOST_PROC=/hostfs/proc"));

    @Setter
    private String imageId;

    private String containerName;
    private List<PortBinding> portBindings;
    private List<Bind> volumeBindings;
    private List<Link> links;
    private List<String> cmds;
    private int ordinal;
    @Setter
    private List<String> environment;

    MetricsAndTracingConfig(String containerName,
                            List<PortBinding> portBindings, List<Bind> volumeBindings,
                            List<Link> links, List<String> cmds, List<String> environment) {
        this.containerName = containerName;
        this.portBindings = portBindings;
        this.volumeBindings = volumeBindings;
        this.links = links;
        this.cmds = cmds;
        this.environment = environment;
        this.ordinal = 2;
    }

    @Override
    public RestartPolicy getRestartPolicy() {
        // This change is made to avoid link dependency on containers.
        // TODO make the process not fail if containers are missing.
        return RestartPolicy.unlessStoppedRestart();
    }
}
