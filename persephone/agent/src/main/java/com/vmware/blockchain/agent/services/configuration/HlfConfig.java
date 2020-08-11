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
public enum HlfConfig implements BaseContainerSpec {

    // HLF Configuration
    HLF_CONCORD("concord", List.of(
            new PortBinding(Ports.Binding.bindPort(50052), ExposedPort.tcp(50052)),
            new PortBinding(Ports.Binding.bindPort(50051), ExposedPort.tcp(50051)),
            new PortBinding(Ports.Binding.bindPort(5458), ExposedPort.tcp(5458)),
            new PortBinding(Ports.Binding.bindPort(3501), ExposedPort.tcp(3501)),
            new PortBinding(Ports.Binding.bindPort(3502), ExposedPort.tcp(3502)),
            new PortBinding(Ports.Binding.bindPort(3503), ExposedPort.tcp(3503)),
            new PortBinding(Ports.Binding.bindPort(3504), ExposedPort.tcp(3504)),
            new PortBinding(Ports.Binding.bindPort(3505), ExposedPort.tcp(3505))),
                ConcordHelper.getDefaultVolBinds(),
                 null),

    HLF_ORDERER("orderer1.example.com", List.of(
            new PortBinding(Ports.Binding.bindPort(7050), ExposedPort.tcp(7050))),
                 null, null),

    HLF_PEER("peer1.org1.example.com", List.of(
            new PortBinding(Ports.Binding.bindPort(7051), ExposedPort.tcp(7051))),
            List.of(Bind.parse("/var/run/:/host/var/run/")),
            null),

    HLF_TOOLS("cli", null, null, null);

    @Setter
    private String imageId;

    private String containerName;
    private List<PortBinding> portBindings;
    private List<Bind> volumeBindings;
    private List<Link> links;
    private int ordinal;

    HlfConfig(String containerName,
               List<PortBinding> portBindings, List<Bind> volumeBindings,
               List<Link> links) {
        this.containerName = containerName;
        this.portBindings = portBindings;
        this.volumeBindings = volumeBindings;
        this.links = links;
        this.ordinal = 1;
    }
}
