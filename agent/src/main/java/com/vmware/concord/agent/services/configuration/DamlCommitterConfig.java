/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent.services.configuration;

import java.util.List;

import com.github.dockerjava.api.model.Bind;
import com.github.dockerjava.api.model.ExposedPort;
import com.github.dockerjava.api.model.Link;
import com.github.dockerjava.api.model.LogConfig;
import com.github.dockerjava.api.model.PortBinding;
import com.github.dockerjava.api.model.Ports;

import lombok.Getter;
import lombok.Setter;

/**
 * Enumeration of [ContainerConfig] for known container images.
 * This should be ultimately moved to manifest.
 */
@Getter
public enum DamlCommitterConfig implements BaseContainerSpec {

    LOGGING(LogConfig.LoggingType.FLUENTD.toString(), null,
            List.of(Bind.parse("/var/lib/docker/containers:/var/lib/docker/containers")),
            null, null, null),

    DAML_EXECUTION_ENGINE("daml_execution_engine", List.of(
            new PortBinding(Ports.Binding.bindPort(55000), ExposedPort.tcp(55000))), null,
                          null, List.of("/doc/daml/kvbc_validator/target/universal/stage/bin/kvbc-validator",
                                        "-J-Xmx4G"), null),
    CONCORD("concord", ConcordHelper.getDefaultPortBindings(),
            ConcordHelper.getDefaultVolBinds(),
            List.of(new Link("daml_execution_engine", "daml_execution_engine")), null, null);

    @Setter
    private String imageId;

    private String containerName;
    private List<PortBinding> portBindings;
    private List<Bind> volumeBindings;
    private List<Link> links;
    private List<String> cmds;
    @Setter
    private List<String> environment;

    DamlCommitterConfig(String containerName,
                        List<PortBinding> portBindings, List<Bind> volumeBindings,
                        List<Link> links, List<String> cmds, List<String> environment) {
        this.containerName = containerName;
        this.portBindings = portBindings;
        this.volumeBindings = volumeBindings;
        this.links = links;
        this.cmds = cmds;
        this.environment = environment;
    }

}
