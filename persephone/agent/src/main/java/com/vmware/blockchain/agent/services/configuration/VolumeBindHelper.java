/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.configuration;

import java.util.List;

import com.github.dockerjava.api.model.Bind;

/**
 * Helper class for volume bindings.
 */
public class VolumeBindHelper {

    /**
     * Default volume bindings.
     * @return list
     */
    public static List<Bind> getConcordVolBinds() {
        return List.of(Bind.parse("/config/concord/config-local:/concord/config-local"),
                       Bind.parse("/config/concord/config-public:/concord/config-public"),
                       Bind.parse("/config/concord/rocksdbdata:/concord/rocksdbdata"),
                       Bind.parse("/config/concord/cores:/concord/cores"));
    }

    // Hack- Remove
    /**
     * Default volume bindings.
     * @return list
     */
    public static List<Bind> getConcordVolBindsDataDisk() {
        return List.of(Bind.parse("/config/concord/config-local:/concord/config-local"),
                       Bind.parse("/config/concord/config-public:/concord/config-public"),
                       Bind.parse("/mnt/sdb/rocksdbdata:/concord/rocksdbdata"),
                       Bind.parse("/config/concord/cores:/concord/cores"));
    }

    /**
     * Default volume bindings.
     * @return list
     */
    public static List<Bind> getIndexdbVolBinds() {
        return List.of(Bind.parse("/config/daml-index-db/db:/var/lib/postgresql/data"));
    }

    /**
     * Default volume bindings.
     * @return list
     */
    public static List<Bind> getIndexdbVolBindsDataDisk() {
        return List.of(Bind.parse("/mnt/sdb/db:/var/lib/postgresql/data"));
    }

}
