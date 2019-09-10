/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent;


import java.util.List;

import com.github.dockerjava.api.model.Bind;
import com.github.dockerjava.api.model.ExposedPort;
import com.github.dockerjava.api.model.Link;
import com.github.dockerjava.api.model.PortBinding;
import com.github.dockerjava.api.model.Ports;

/**
 * Enumeration of [ContainerConfig] for known container images.
 * This should be ultimately moved to manifest.
 */
public enum ContainerConfig {
    CONCORD("vmwblockchain/concord-core", "concord", List.of(
            new PortBinding(Ports.Binding.bindPort(5458), ExposedPort.tcp(5458)),
            new PortBinding(Ports.Binding.bindPort(3501), ExposedPort.tcp(3501)),
            new PortBinding(Ports.Binding.bindPort(3502), ExposedPort.tcp(3502)),
            new PortBinding(Ports.Binding.bindPort(3503), ExposedPort.tcp(3503)),
            new PortBinding(Ports.Binding.bindPort(3504), ExposedPort.tcp(3504)),
            new PortBinding(Ports.Binding.bindPort(3505), ExposedPort.tcp(3505))),
            List.of(Bind.parse("/config/concord/config-local:/concord/config-local"),
                    Bind.parse("/config/concord/config-public:/concord/config-public")),
            null, null, null),
    ETHEREUM_API("vmwblockchain/ethrpc", "ethrpc",
                 List.of(new PortBinding(Ports.Binding.bindPort(8545), ExposedPort.tcp(8545))),
                 null, List.of(new Link("concord", "concord")), List.of(
            "/bin/bash", "-c", "sed -i s/localhost/concord/g application.properties && java -jar concord-ethrpc.jar"
    ), null),

    // DAML Configuration

    DAML_EXECUTION_ENGINE("vmwblockchain/daml-execution-engine", "daml_execution_engine", List.of(
            new PortBinding(Ports.Binding.bindPort(55000), ExposedPort.tcp(55000))), null,
                          null, List.of("/doc/daml/kvbc_validator/target/universal/stage/bin/kvbc-validator"), null),
    DAML_CONCORD("vmwblockchain/concord-core", "concord", List.of(
            new PortBinding(Ports.Binding.bindPort(50051), ExposedPort.tcp(50051)),
            new PortBinding(Ports.Binding.bindPort(5458), ExposedPort.tcp(5458)),
            new PortBinding(Ports.Binding.bindPort(3501), ExposedPort.tcp(3501)),
            new PortBinding(Ports.Binding.bindPort(3502), ExposedPort.tcp(3502)),
            new PortBinding(Ports.Binding.bindPort(3503), ExposedPort.tcp(3503)),
            new PortBinding(Ports.Binding.bindPort(3504), ExposedPort.tcp(3504)),
            new PortBinding(Ports.Binding.bindPort(3505), ExposedPort.tcp(3505))),
                 List.of(Bind.parse("/config/concord/config-local:/concord/config-local"),
                         Bind.parse("/config/concord/config-public:/concord/config-public")),
                 List.of(new Link("daml_execution_engine", "daml_execution_engine")), null, null),

    DAML_INDEX_DB("vmwblockchain/daml-index-db", "daml_index_db", List.of(
            new PortBinding(Ports.Binding.bindPort(5432), ExposedPort.tcp(5432))),
                  List.of(Bind.parse("/config/daml_index_db/daml_index_db:/var/lib/postgresql/data")),
                  null,
                  List.of("postgres", "-c", "max_connections=300", "-c", "shared_buffers=80MB"),
                  List.of("POSTGRES_USER=indexdb", "POSTGRES_MULTIPLE_DATABASES=daml_ledger_api")),

    DAML_LEDGER_API("vmwblockchain/daml-ledger-api", "daml_ledger_api", List.of(
            new PortBinding(Ports.Binding.bindPort(6865), ExposedPort.tcp(6865))), null,
                    List.of(new Link("concord", "concord"),
                            new Link("daml_index_db", "daml_index_db")),
                    List.of("/doc/daml/kvbc_ledger_server/target/universal/stage/bin/kvbc-ledger-server",
                            "concord:50051", "daml_ledger_api"),
                    List.of("INDEXDB_HOST=daml_index_db",
                            "INDEXDB_PORT=5432",
                            "INDEXDB_USER=indexdb",
                            "CONCORD_HOST=concord",
                            "CONCORD_PORT=50051",
                            "PARTICIPANT_ID=daml_ledger_api"));

    String imageRepository;
    String imageId;
    String containerName;
    List<PortBinding> portBindings;
    List<Bind> volumeBindings;
    List<Link> links;
    List<String> cmds;
    List<String> environment;


    ContainerConfig(String imageRepository, String containerName,
                    List<PortBinding> portBindings, List<Bind> volumeBindings,
                    List<Link> links, List<String> cmds, List<String> environment) {
        this.imageRepository = imageRepository;
        this.containerName = containerName;
        this.portBindings = portBindings;
        this.volumeBindings = volumeBindings;
        this.links = links;
        this.cmds = cmds;
        this.environment = environment;
    }
}