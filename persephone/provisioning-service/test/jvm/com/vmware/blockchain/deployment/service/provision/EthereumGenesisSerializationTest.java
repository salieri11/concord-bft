/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.provision;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.stream.Collectors;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.ethereum.type.Genesis;
import com.vmware.blockchain.protobuf.kotlinx.serialization.ProtoBuf;

import kotlinx.serialization.json.Json;
import kotlinx.serialization.json.JsonConfiguration;
import kotlinx.serialization.modules.EmptyModule;
import net.jqwik.api.Arbitraries;
import net.jqwik.api.Arbitrary;
import net.jqwik.api.Combinators;
import net.jqwik.api.ForAll;
import net.jqwik.api.Property;
import net.jqwik.api.Provide;

/**
 * Various unit-test testing serialization correctness of {@link Genesis}.
 */
class EthereumGenesisSerializationTest {

    /** Static array of hexadecimal digits. */
    private static char[] hexdigits = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};

    private static Json json = new Json(JsonConfiguration.getDefault(), EmptyModule.INSTANCE);
    private static ProtoBuf protobuf = new ProtoBuf();

    /**
     * Provider of arbitrary {@link Genesis} values with shrinking support.
     */
    @Provide
    Arbitrary<Genesis> geneses() {
        var chainId = Arbitraries.integers().between(0, Integer.MAX_VALUE);
        var homesteadBlock = Arbitraries.integers().between(0, Integer.MAX_VALUE);
        var eip155Block = Arbitraries.integers().between(0, Integer.MAX_VALUE);
        var eip158Block = Arbitraries.integers().between(0, Integer.MAX_VALUE);
        var config = Combinators.combine(chainId, homesteadBlock, eip155Block, eip158Block)
                .as(Genesis.Config::new);
        var walletAddress = Arbitraries.strings().withChars(hexdigits).ofLength(40);
        var wallet = Arbitraries.integers().greaterOrEqual(0)
                .map(Object::toString)
                .map(Genesis.Wallet::new);
        var alloc = Combinators.combine(walletAddress, wallet).as(Map::entry).list().ofMaxSize(100)
                .map(list -> list.stream()
                        .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue))
                );
        var nonce = Arbitraries.longs()
                .map(Long::toHexString)
                .map(value -> "0x" + value);
        var difficulty = Arbitraries.integers().greaterOrEqual(0)
                .map(Integer::toHexString)
                .map(value -> "0x" + value);
        var mixhash = Arbitraries.strings().withChars(hexdigits).ofLength(64)
                .map(value -> "0x" + value);
        var parentHash = Arbitraries.strings().withChars(hexdigits).ofLength(64)
                .map(value -> "0x" + value);
        var gasLimit = Arbitraries.integers().greaterOrEqual(0)
                .map(Integer::toHexString)
                .map(value -> "0x" + value);

        return Combinators.combine(config, nonce, difficulty, mixhash, parentHash, gasLimit, alloc)
                .as(Genesis::new);
    }

    /**
     * Test that round-trip Protocol Buffer serialization of random {@link Genesis} does not lose
     * nor create additional information.
     */
    @Property
    void protocolBufferRoundTrip(@ForAll("geneses") Genesis value) {
        var serialized = protobuf.dump(Genesis.getSerializer(), value);
        var deserialized = protobuf.load(Genesis.getSerializer(), serialized);
        Assertions.assertThat(deserialized).isEqualTo(value);
    }

    /**
     * Test that round-trip Protocol Buffer serialization of random {@link Genesis} does not lose
     * nor create additional information.
     */
    @Property
    void jsonRoundTrip(@ForAll("geneses") Genesis value) {
        var serialized = json.stringify(Genesis.getSerializer(), value);
        var deserialized = json.parse(Genesis.getSerializer(), serialized);
        Assertions.assertThat(deserialized).isEqualTo(value);
    }

    /**
     * Test that the included resource file "exampleGenesis.json" deserializes correctly as
     * expected.
     *
     * @throws IOException
     *   if resource file cannot be found or read.
     */
    @Test
    void exampleGenesisJsonSerialization() throws IOException {
        try (InputStream inputStream =
                     getClass().getClassLoader().getResourceAsStream("exampleGenesis.json")
        ) {
            Assertions.assertThat(inputStream).isNotNull();

            var value = new String(inputStream.readAllBytes(), StandardCharsets.UTF_8);
            var deserialized = json.parse(Genesis.getSerializer(), value);

            // Verify config section.
            Assertions.assertThat(deserialized.getConfig().getChainId()).isEqualTo(1);
            Assertions.assertThat(deserialized.getConfig().getHomesteadBlock()).isEqualTo(0);
            Assertions.assertThat(deserialized.getConfig().getEip155Block()).isEqualTo(0);
            Assertions.assertThat(deserialized.getConfig().getEip158Block()).isEqualTo(0);

            // Verify alloc section.
            var key1 = "262c0d7ab5ffd4ede2199f6ea793f819e1abb019";
            var key2 = "5bb088f57365907b1840e45984cae028a82af934";
            var key3 = "0000a12b3f3d6c9b0d3f126a83ec2dd3dad15f39";
            var balance1 = "12345";
            var balance2 = "0xabcdef";
            var balance3 = "0x7fffffffffffffff";
            var alloc = Map.of(
                    key1, new Genesis.Wallet(balance1),
                    key2, new Genesis.Wallet(balance2),
                    key3, new Genesis.Wallet(balance3)
            );
            Assertions.assertThat(deserialized.getAlloc().size()).isEqualTo(3);
            Assertions.assertThat(deserialized.getAlloc()).containsAllEntriesOf(alloc);

            // Verify individual fields.
            var zeroHash = "0x0000000000000000000000000000000000000000000000000000000000000000";
            Assertions.assertThat(deserialized.getNonce()).isEqualTo("0x0000000000000000");
            Assertions.assertThat(deserialized.getDifficulty()).isEqualTo("0x400");
            Assertions.assertThat(deserialized.getMixhash()).isEqualTo(zeroHash);
            Assertions.assertThat(deserialized.getParentHash()).isEqualTo(zeroHash);
            Assertions.assertThat(deserialized.getGasLimit()).isEqualTo("0xf4240");

            // Perform a quick round-trip and compare the round-tripped value with initial
            // deserialized value.
            var serialized = json.stringify(Genesis.getSerializer(), deserialized);
            var roundTrip = json.parse(Genesis.getSerializer(), serialized);
            Assertions.assertThat(roundTrip).isEqualTo(deserialized);
        }
    }
}
