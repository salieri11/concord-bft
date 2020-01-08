/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import com.vmware.blockchain.ethereum.type.Genesis;

import kotlinx.serialization.UpdateMode;
import kotlinx.serialization.json.Json;
import kotlinx.serialization.json.JsonConfiguration;
import kotlinx.serialization.modules.EmptyModule;

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
    public String getGenesis(Genesis genesis) {

        var json = getJsonConfig();
        var genesisJson = json.toJson(Genesis.getSerializer(), genesis);
        return genesisJson.toString();
    }

    /**
     * Helper Utility to get json configuration.
     * @return Json object for json parsing
     */
    static Json getJsonConfig() {
        return new Json(
                new JsonConfiguration(
                        false, /* encodeDefaults */
                        true, /* strictMode */
                        false, /* unquoted */
                        false, /* allowStructuredMapKeys */
                        false, /* prettyPrint */
                        "    ", /* indent */
                        false, /* useArrayPolymorphism */
                        "type", /* classDiscriminator */
                        UpdateMode.OVERWRITE /* updateMode */
                ),
                EmptyModule.INSTANCE
        );
    }
}
