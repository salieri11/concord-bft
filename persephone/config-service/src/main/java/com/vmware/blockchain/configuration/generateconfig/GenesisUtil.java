/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import com.google.protobuf.util.JsonFormat;
import com.vmware.blockchain.ethereum.type.Genesis;

/**
 * Utility class for generating the input for Genesis Json file.
 */
public class GenesisUtil {

    /**
     * file path.
     */
    public static final String genesisPath = "/concord/config-public/genesis.json";

    /**
     * Utility to generate concord config.
     * @return json string of genesis block
     */
    @Deprecated
    public String getGenesis(Genesis genesis) {
        try {
            return JsonFormat.printer().print(genesis);
        } catch (Exception e) {
            return "Error parsing genesis block";
        }
    }
}
