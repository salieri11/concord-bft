/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent;


import java.util.List;
import java.util.Map;

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
            new PortBinding(Ports.Binding.bindPort(5458), ExposedPort.tcp(5458)),
            new PortBinding(Ports.Binding.bindPort(3501), ExposedPort.tcp(3501)),
            new PortBinding(Ports.Binding.bindPort(3502), ExposedPort.tcp(3502)),
            new PortBinding(Ports.Binding.bindPort(3503), ExposedPort.tcp(3503)),
            new PortBinding(Ports.Binding.bindPort(3504), ExposedPort.tcp(3504)),
            new PortBinding(Ports.Binding.bindPort(3505), ExposedPort.tcp(3505))),
                      List.of(Bind.parse("/config/concord/config-local:/concord/config-local"),
                              Bind.parse("/config/concord/config-public:/concord/config-public")),
                      List.of(new Link("daml_execution_engine", "daml_execution_engine")), null, null),

    DAML_LEDGER_API("vmwblockchain/daml-ledger-api", "daml_ledger_api", List.of(
            new PortBinding(Ports.Binding.bindPort(6865), ExposedPort.tcp(6865))), null,
                    List.of(new Link("concord", "concord")),
                    List.of("/doc/daml/kvbc_ledger_server/target/universal/stage/bin/kvbc-ledger-server",
                            "concord:5458", "daml_ledger_api"), null);

    String imageRepository;
    String imageId;
    String containerName;
    List<PortBinding> portBindings;
    List<Bind> volumeBindings;
    List<Link> links;
    List<String> cmds;
    Map<String, String> environment;


    ContainerConfig(String imageRepository, String containerName,
                    List<PortBinding> portBindings, List<Bind> volumeBindings,
                    List<Link> links, List<String> cmds, Map<String, String> environment) {
        this.imageRepository = imageRepository;
        this.containerName = containerName;
        this.portBindings = portBindings;
        this.volumeBindings = volumeBindings;
        this.links = links;
        this.cmds = cmds;
        this.environment = environment;
    }
}