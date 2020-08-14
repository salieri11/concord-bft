/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.configuration;

import java.util.List;

import com.github.dockerjava.api.model.Bind;

/**
 * Heloer class for concord container specs.
 */
public class ConcordHelper {

    /**
     * Default volume bindings.
     * @return list
     */
    public static List<Bind> getDefaultVolBinds() {
        return List.of(Bind.parse("/config/concord/config-local:/concord/config-local"),
                       Bind.parse("/config/concord/config-public:/concord/config-public"),
                       Bind.parse("/config/concord/rocksdbdata:/concord/rocksdbdata"),
                       Bind.parse("/config/concord/cores:/concord/cores"));
    }
}
