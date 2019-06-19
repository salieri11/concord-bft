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

import com.vmware.blockchain.deployment.service.testutilitilies.TestUtil;
import com.vmware.blockchain.deployment.service.util.Constants;

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
        ConcordConfigUtil util = new ConcordConfigUtil();
        String config = util.generateConfigUtil(hostIps);
        ClassLoader classLoader = getClass().getClassLoader();
        InputStream expectedStream = classLoader.getResourceAsStream("SampleFourNodeConcordConfig.yaml");
        Assertions.assertThat(config).isEqualTo(TestUtil.readFromInputStream(expectedStream));

        assert util.nodePrincipal.size() == 4;
        assert util.maxPrincipalId == (hostIps.size() + Constants.CLIENT_PROXY_PER_NODE * hostIps.size()) - 1;
        for (int node : util.nodePrincipal.keySet()) {
            assert util.nodePrincipal.get(node).size() == 4;
        }

    }

    @Test
    void testConfigUtilNegative() {
        ConcordConfigUtil util = new ConcordConfigUtil();
        Assertions.assertThat(util
                .generateConfigUtil(null)).isBlank();
    }

    @Test
    void testConfigUtilInvalidNode() {
        List<String> hostIps = new ArrayList<String>();
        hostIps.add("10.0.0.1");
        hostIps.add("10.0.0.2");
        hostIps.add("10.0.0.3");
        ConcordConfigUtil util = new ConcordConfigUtil();
        Assertions.assertThat(util
                .generateConfigUtil(hostIps)).isBlank();
    }

    @Test
    void testConfigUtilInvalidConfig() {
        List<String> hostIps = new ArrayList<String>();
        hostIps.add("10.0.0.1");
        hostIps.add("10.0.0.2");
        hostIps.add("10.0.0.3");
        hostIps.add("10.0.0.4");
        ConcordConfigUtil util = new ConcordConfigUtil();
        Assertions.assertThat(util
                .generateConfigUtil(hostIps, 1, 2)).isBlank();
    }

    @Test
    void testConfigUtilDefaultSamplePositive() throws IOException {
        List<String> hostIps = new ArrayList<String>();
        hostIps.add("concord1");
        hostIps.add("concord2");
        hostIps.add("concord3");
        hostIps.add("concord4");
        ConcordConfigUtil util = new ConcordConfigUtil();
        String config = util.generateConfigUtil(hostIps);
        ClassLoader classLoader = getClass().getClassLoader();
        InputStream expectedStream = classLoader.getResourceAsStream("SampleFourNodeConcordNamedConfig.yaml");
        Assertions.assertThat(config).isEqualTo(TestUtil.readFromInputStream(expectedStream));

        assert util.nodePrincipal.size() == 4;
        assert util.maxPrincipalId == (hostIps.size() + Constants.CLIENT_PROXY_PER_NODE * hostIps.size()) - 1;
        for (int node : util.nodePrincipal.keySet()) {
            assert util.nodePrincipal.get(node).size() == 4;
        }
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
        ConcordConfigUtil util = new ConcordConfigUtil();
        String config = util.generateConfigUtil(hostIps);
        ClassLoader classLoader = getClass().getClassLoader();
        InputStream expectedStream = classLoader.getResourceAsStream("SampleSevenNodeConcordConfig.yaml");
        Assertions.assertThat(config).isEqualTo(TestUtil.readFromInputStream(expectedStream));

        assert util.nodePrincipal.size() == 7;
        assert util.maxPrincipalId == (hostIps.size() + Constants.CLIENT_PROXY_PER_NODE * hostIps.size()) - 1;
        for (int node : util.nodePrincipal.keySet()) {
            assert util.nodePrincipal.get(node).size() == 4;
        }
    }
}

