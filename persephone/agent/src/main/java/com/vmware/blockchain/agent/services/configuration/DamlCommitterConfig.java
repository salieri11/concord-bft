/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.configuration;

import java.util.List;

import com.github.dockerjava.api.model.Bind;
import com.github.dockerjava.api.model.ExposedPort;
import com.github.dockerjava.api.model.Link;
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

    DAML_EXECUTION_ENGINE("daml_execution_engine", List.of(
            new PortBinding(Ports.Binding.bindPort(55000), ExposedPort.tcp(55000))), null,
                          null),
    DAML_CONCORD("concord", List.of(
            new PortBinding(Ports.Binding.bindPort(50051), ExposedPort.tcp(50051)),
            new PortBinding(Ports.Binding.bindPort(5458), ExposedPort.tcp(5458)),
            new PortBinding(Ports.Binding.bindPort(3501), ExposedPort.tcp(3501)),
            new PortBinding(Ports.Binding.bindPort(9891), ExposedPort.tcp(9891)),
            new PortBinding(Ports.Binding.bindIpAndPort("127.0.0.1", 6888), ExposedPort.tcp(6888))),
                 VolumeBindHelper.getConcordVolBinds(),
                 List.of(new Link("daml_execution_engine", "daml_execution_engine")));

    @Setter
    private String imageId;

    private String containerName;
    private List<PortBinding> portBindings;

    @Setter
    private List<Bind> volumeBindings;
    private List<Link> links;
    private int ordinal;

    DamlCommitterConfig(String containerName,
                        List<PortBinding> portBindings, List<Bind> volumeBindings,
                        List<Link> links) {
        this.containerName = containerName;
        this.portBindings = portBindings;
        this.volumeBindings = volumeBindings;
        this.links = links;
        this.ordinal = 1;
    }

}
