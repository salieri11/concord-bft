/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.deployment.v1.NodesInfo;

/**
 * Test for DamlIndexDbUtil.
 */
public class DamlIndexDbUtilTest {

    @Test
    public void testHappyPath() throws IOException {

        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder().setId("TEST-NODE").build();
        String actual = new DamlIndexDbUtil().generateConfig(nodeInfo);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleDamlIndexDbConfig.txt").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        Assertions.assertThat(actual.equals(expected)).isTrue();
    }

    @Test
    public void testPath() {
        Assertions.assertThat(DamlIndexDbUtil.envVarPath.equals("/daml-index-db/environment-vars")).isTrue();
    }
}
