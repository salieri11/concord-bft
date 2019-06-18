/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.generateconfig;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.deployment.service.util.TestUtil;

/**
 * ConfigYaml Unit test configuration.
 */
public class ConcordConfigUtilTest {

    @Test
    void testConfigUtilPositive() throws IOException {
        List<String> hostIps = new ArrayList<String>();
        hostIps.add("10.0.0.1");
        hostIps.add("10.0.0.2");
        hostIps.add("10.0.0.3");
        hostIps.add("10.0.0.4");
        String config = ConcordConfigUtil.generateConfigUtil(hostIps);
        ClassLoader classLoader = getClass().getClassLoader();
        InputStream expectedStream = classLoader.getResourceAsStream("SampleFourNodeConcordConfig.yaml");
        Assertions.assertThat(config).isEqualTo(TestUtil.readFromInputStream(expectedStream));
    }

    @Test
    void testConfigUtilNegative() {
        Assertions.assertThat(ConcordConfigUtil
                .generateConfigUtil(null)).isEqualTo(null);
    }

    @Test
    void testConfigUtilInvalidNode() {
        List<String> hostIps = new ArrayList<String>();
        hostIps.add("10.0.0.1");
        hostIps.add("10.0.0.2");
        hostIps.add("10.0.0.3");
        Assertions.assertThat(ConcordConfigUtil
                .generateConfigUtil(hostIps)).isEqualTo(null);
    }

    @Test
    void testConfigUtilInvalidConfig() {
        List<String> hostIps = new ArrayList<String>();
        hostIps.add("10.0.0.1");
        hostIps.add("10.0.0.2");
        hostIps.add("10.0.0.3");
        hostIps.add("10.0.0.4");
        Assertions.assertThat(ConcordConfigUtil
                .generateConfigUtil(hostIps, 1, 2)).isEqualTo(null);
    }

    @Test
    void testConfigUtilDefaultSamplePositive() throws IOException {
        List<String> hostIps = new ArrayList<String>();
        hostIps.add("concord1");
        hostIps.add("concord2");
        hostIps.add("concord3");
        hostIps.add("concord4");
        String config = ConcordConfigUtil.generateConfigUtil(hostIps);
        ClassLoader classLoader = getClass().getClassLoader();
        InputStream expectedStream = classLoader.getResourceAsStream("SampleFourNodeConcordNamedConfig.yaml");
        Assertions.assertThat(config).isEqualTo(TestUtil.readFromInputStream(expectedStream));
    }

    @Test
    void testConfigUtilSevenPositive() throws IOException {
        List<String> hostIps = new ArrayList<String>();
        hostIps.add("10.0.0.1");
        hostIps.add("10.0.0.2");
        hostIps.add("10.0.0.3");
        hostIps.add("10.0.0.4");
        hostIps.add("10.0.0.5");
        hostIps.add("10.0.0.6");
        hostIps.add("10.0.0.7");
        String config = ConcordConfigUtil.generateConfigUtil(hostIps);
        ClassLoader classLoader = getClass().getClassLoader();
        InputStream expectedStream = classLoader.getResourceAsStream("SampleSevenNodeConcordConfig.yaml");
        Assertions.assertThat(config).isEqualTo(TestUtil.readFromInputStream(expectedStream));
    }
}

