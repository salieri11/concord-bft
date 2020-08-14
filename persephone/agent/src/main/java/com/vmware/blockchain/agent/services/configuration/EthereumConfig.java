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
public enum EthereumConfig implements BaseContainerSpec {

    CONCORD("concord", List.of(
            new PortBinding(Ports.Binding.bindPort(50051), ExposedPort.tcp(50051)),
            new PortBinding(Ports.Binding.bindPort(5458), ExposedPort.tcp(5458)),
            new PortBinding(Ports.Binding.bindPort(3501), ExposedPort.tcp(3501)),
            new PortBinding(Ports.Binding.bindPort(3502), ExposedPort.tcp(3502)),
            new PortBinding(Ports.Binding.bindPort(3503), ExposedPort.tcp(3503)),
            new PortBinding(Ports.Binding.bindPort(3504), ExposedPort.tcp(3504)),
            new PortBinding(Ports.Binding.bindPort(3505), ExposedPort.tcp(3505)),
            new PortBinding(Ports.Binding.bindPort(9891), ExposedPort.tcp(9891))),
            ConcordHelper.getDefaultVolBinds(),
            null, null),
    ETHEREUM_API("ethrpc",
            List.of(new PortBinding(Ports.Binding.bindPort(8545), ExposedPort.tcp(8545))),
            null,
            List.of(new Link("concord", "concord")),
            List.of("CONCORD_AUTHORITIES=concord:5458"));

    @Setter
    private String imageId;

    private String containerName;
    private List<PortBinding> portBindings;
    private List<Bind> volumeBindings;
    private List<Link> links;
    private int ordinal;

    // TODO Move this to Config Service
    private List<String> environment;

    EthereumConfig(String containerName,
                   List<PortBinding> portBindings, List<Bind> volumeBindings,
                   List<Link> links, List<String> environment) {
        this.containerName = containerName;
        this.portBindings = portBindings;
        this.volumeBindings = volumeBindings;
        this.links = links;
        this.environment = environment;
        this.ordinal = 1;
    }

    public List<String> getEnvironment() {
        return environment;
    }
}
