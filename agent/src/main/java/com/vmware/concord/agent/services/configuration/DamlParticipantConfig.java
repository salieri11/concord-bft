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
public enum DamlParticipantConfig implements BaseContainerSpec {

    LOGGING(LogConfig.LoggingType.FLUENTD.toString(), null,
            List.of(Bind.parse("/var/lib/docker/containers:/var/lib/docker/containers")),
            null, null, null),

    DAML_INDEX_DB("daml_index_db", List.of(
            new PortBinding(Ports.Binding.bindPort(5432), ExposedPort.tcp(5432))),
                  List.of(Bind.parse("/config/daml_index_db/daml_index_db:/var/lib/postgresql/data")),
                  null,
                  List.of("postgres", "-c", "max_connections=300", "-c", "shared_buffers=80MB"),
                  List.of("POSTGRES_USER=indexdb", "POSTGRES_MULTIPLE_DATABASES=daml_ledger_api",
                          "BUFFER_SIZE=80MB", "MAX_CONNECTIONS=300")),

    DAML_LEDGER_API("daml_ledger_api", List.of(
            new PortBinding(Ports.Binding.bindPort(6865), ExposedPort.tcp(6865))), null,
                    List.of(new Link("daml_index_db", "daml_index_db")),
                    null,
                    List.of("INDEXDB_HOST=daml_index_db",
                            "INDEXDB_PORT=5432",
                            "INDEXDB_USER=indexdb",
                            "CONCORD_HOST=concord",
                            "CONCORD_PORT=50051",
                            "PARTICIPANT_ID=daml_ledger_api",
                            "JAVA_OPTS=-Xmx4G"));

    //TODO Inject Concord IP

    @Setter
    private String imageId;

    private String containerName;
    private List<PortBinding> portBindings;
    private List<Bind> volumeBindings;
    private List<Link> links;
    private List<String> cmds;
    @Setter
    private List<String> environment;

    DamlParticipantConfig(String containerName,
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
