/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.configuration;

import java.util.Map;

import com.google.protobuf.util.JsonFormat;
import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.ethereum.type.Genesis;

import lombok.extern.slf4j.Slf4j;

/**
 * This class provides the configuration required for ETHEREUM.
 */
@Slf4j
public class EthereumConfiguration {

    /**
     * Static method that generates the Genesis block object.
     *
     * @return genesis
     */
    public static String getGenesisObject() {
        try {
            return JsonFormat.printer().print(Genesis.newBuilder().setConfig(
                    Genesis.Config.newBuilder()
                            .setChainId(1)
                            .setHomesteadBlock(0)
                            .setEip155Block(0)
                            .setEip158Block(0)
                            .build()
            )
                                                      .setNonce("0x0000000000000000")
                                                      .setDifficulty("0x400")
                                                      .setMixhash(
                                                              "0x000000000000000000000000000000000000000000000"
                                                              + "0000000000000000000")
                                                      .setParentHash(
                                                              "0x000000000000000000000000000000000000000000000000"
                                                              + "0000000000000000")
                                                      .setGasLimit("0xf4240")
                                                      .putAllAlloc(Map.of(
                                                              "262c0d7ab5ffd4ede2199f6ea793f819e1abb019",
                                                              Genesis.Wallet.newBuilder()
                                                                      .setBalance("12345").build(),
                                                              "5bb088f57365907b1840e45984cae028a82af934",
                                                              Genesis.Wallet.newBuilder()
                                                                      .setBalance("0xabcdef").build(),
                                                              "0000a12b3f3d6c9b0d3f126a83ec2dd3dad15f39",
                                                              Genesis.Wallet.newBuilder()
                                                                      .setBalance("0x7fffffffffffffff").build()
                                                      ))
                                                      .build());
        } catch (Exception e) {
            throw new PersephoneException("Error generating genesis block.");
        }
    }
}