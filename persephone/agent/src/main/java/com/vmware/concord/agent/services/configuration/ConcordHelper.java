/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent.services.configuration;

import java.util.List;

import com.github.dockerjava.api.model.Bind;
import com.github.dockerjava.api.model.ExposedPort;
import com.github.dockerjava.api.model.PortBinding;
import com.github.dockerjava.api.model.Ports;

/**
 * Heloer class for concord container specs.
 */
public class ConcordHelper {

    /**
     * Default port bindings.
     * @return list
     */
    public static List<PortBinding> getDefaultPortBindings() {
        return List.of(
                new PortBinding(Ports.Binding.bindPort(50051), ExposedPort.tcp(50051)),
                new PortBinding(Ports.Binding.bindPort(5458), ExposedPort.tcp(5458)),
                new PortBinding(Ports.Binding.bindPort(3501), ExposedPort.tcp(3501)),
                new PortBinding(Ports.Binding.bindPort(3502), ExposedPort.tcp(3502)),
                new PortBinding(Ports.Binding.bindPort(3503), ExposedPort.tcp(3503)),
                new PortBinding(Ports.Binding.bindPort(3504), ExposedPort.tcp(3504)),
                new PortBinding(Ports.Binding.bindPort(3505), ExposedPort.tcp(3505)),
                new PortBinding(Ports.Binding.bindPort(9891), ExposedPort.tcp(9891)));
    }

    /**
     * Default volume bindings.
     * @return list
     */
    public static List<Bind> getDefaultVolBinds() {
        return List.of(Bind.parse("/config/concord/config-local:/concord/config-local"),
                Bind.parse("/config/concord/config-public:/concord/config-public"));
    }
}
