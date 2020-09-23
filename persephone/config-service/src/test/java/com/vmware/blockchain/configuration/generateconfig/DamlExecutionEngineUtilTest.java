/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test for DamlExecutionEngineUtil.
 */
public class DamlExecutionEngineUtilTest {

    @Test
    public void testHappyPath() throws IOException {

        String actual = new DamlExecutionEngineUtil().generateConfig();

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleDamlExecutionEngineConfig.txt").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        Assertions.assertThat(actual.equals(expected));
    }

    @Test
    public void testPath() {
        Assertions.assertThat(Constants.DAML_ENV_VARS_PATH
                                      .equals("/daml-execution-engine/environment-vars"))
                .isTrue();
    }
}
