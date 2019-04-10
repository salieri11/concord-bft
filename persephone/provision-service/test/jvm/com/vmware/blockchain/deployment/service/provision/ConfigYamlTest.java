/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.provision;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.nio.file.DirectoryNotEmptyException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

/**
 * ConfigYaml Unit test configuration.
 */
public class ConfigYamlTest {

    @Test
    void testConfigUtilPositive() {
        ConfigYaml config = new ConfigYaml("./testYaml");
        List<String> hostIps = new ArrayList<String>();
        hostIps.add("10.0.0.1");
        hostIps.add("10.0.0.2");
        hostIps.add("10.0.0.3");
        hostIps.add("10.0.0.4");
        Assertions.assertThat(config.generateConfigUtil(hostIps)).isTrue();
    }

    @Test
    void testConfigUtilNegative() {
        ConfigYaml config = new ConfigYaml("./testYaml");
        Assertions.assertThat(config.generateConfigUtil(null)).isFalse();
    }

    @Test
    void testConfigUtilInvalidNode() {
        ConfigYaml config = new ConfigYaml("./testYaml");
        List<String> hostIps = new ArrayList<String>();
        hostIps.add("10.0.0.1");
        hostIps.add("10.0.0.2");
        hostIps.add("10.0.0.3");
        Assertions.assertThat(config.generateConfigUtil(hostIps)).isFalse();
    }

    @Test
    void testConfigUtilInvalidConfig() {
        ConfigYaml config = new ConfigYaml("./testYaml");
        List<String> hostIps = new ArrayList<String>();
        hostIps.add("10.0.0.1");
        hostIps.add("10.0.0.2");
        hostIps.add("10.0.0.3");
        hostIps.add("10.0.0.4");
        Assertions.assertThat(config.generateConfigUtil(hostIps, 1, 2)).isFalse();
    }

    @Test
    void testConfigUtilDefaultSamplePositive() {
        ConfigYaml config = new ConfigYaml("./concordYaml");
        List<String> hostIps = new ArrayList<String>();
        hostIps.add("concord1");
        hostIps.add("concord2");
        hostIps.add("concord3");
        hostIps.add("concord4");
        Assertions.assertThat(config.generateConfigUtil(hostIps)).isTrue();
        // comparing the content of this file with what i have in resource folder would be good ASSERTION
    }
    
    @AfterEach
    void cleanup() {
        try { 
            Files.deleteIfExists(Paths.get("./concordYaml"));
            Files.deleteIfExists(Paths.get("./testYaml"));
        } catch(NoSuchFileException e) { 
        } catch(DirectoryNotEmptyException e) { 
        } catch(IOException e) { 
        } 
    }
}

