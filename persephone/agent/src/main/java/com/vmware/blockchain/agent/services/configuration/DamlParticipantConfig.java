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
public enum DamlParticipantConfig implements BaseContainerSpec {

    DAML_INDEX_DB("daml_index_db", null,
                  VolumeBindHelper.getIndexdbVolBinds(),
                  null),

    DAML_LEDGER_API("daml_ledger_api", List.of(
            new PortBinding(Ports.Binding.bindPort(6865), ExposedPort.tcp(6865))), null,
                    List.of(new Link("daml_index_db", "daml_index_db"))),

    CONCORD_OPERATOR("concord_operator", null, null, null, true);

    @Setter
    private String imageId;

    private String containerName;
    private List<PortBinding> portBindings;

    @Setter
    private List<Bind> volumeBindings;
    private List<Link> links;
    private int ordinal;

    private boolean downloadImageOnly;

    DamlParticipantConfig(String containerName,
                          List<PortBinding> portBindings, List<Bind> volumeBindings, List<Link> links) {
        this.containerName = containerName;
        this.portBindings = portBindings;
        this.volumeBindings = volumeBindings;
        this.links = links;
        this.ordinal = 1;
    }

    DamlParticipantConfig(String containerName,
                          List<PortBinding> portBindings, List<Bind> volumeBindings, List<Link> links,
                          boolean downloadImageOnly) {
        this(containerName, portBindings, volumeBindings, links);
        this.downloadImageOnly = downloadImageOnly;
    }
}
