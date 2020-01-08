/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.util.Map;

import org.junit.jupiter.api.Test;

import com.vmware.blockchain.ethereum.type.Genesis;

/**
 * GenesisUtil Unit test.
 */
public class GenesisUtilTest {

    @Test
    void testGenesisGenerationString() {
        Genesis genesisInput = new Genesis(
                new Genesis.Config(1, 0, 0, 0),
                "0x0000000000000000",
                "0x400",
                "0x0000000000000000000000000000000000000000000000000000000000000000",
                "0x0000000000000000000000000000000000000000000000000000000000000000",
                "0xf4240",
                Map.of(
                        "262c0d7ab5ffd4ede2199f6ea793f819e1abb019", new Genesis.Wallet("12345"),
                        "5bb088f57365907b1840e45984cae028a82af934", new Genesis.Wallet("0xabcdef"),
                        "0000a12b3f3d6c9b0d3f126a83ec2dd3dad15f39", new Genesis.Wallet("0x7fffffffffffffff")
                ));

        var util = new GenesisUtil();
        var result = util.getGenesis(genesisInput);

        var json = GenesisUtil.getJsonConfig();
        var genesisOutput = json.parse(Genesis.getSerializer(), result);

        assert genesisOutput.equals(genesisInput);
    }
}
