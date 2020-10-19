/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.provision;

import static org.mockito.Mockito.mock;

import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.deployment.services.orchestrationsite.OrchestrationSites;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;

/**
 * Tests for the concord config provider.
 */
@ExtendWith(SpringExtension.class)
public class OrchestrationSitesTests {

    OrchestrationSiteInfo originalSiteInfo;

    /**
     * Initialize various mocks.
     */
    @BeforeEach
    public void init() {
        originalSiteInfo = mock(OrchestrationSiteInfo.class);
    }

    @Test
    void testBuildSiteInfo() {
        OrchestrationSiteInfo siteInfo =
                OrchestrationSites.buildSiteInfo(originalSiteInfo, Endpoint.newBuilder().build(),
                                                 Endpoint.newBuilder().build());

        Assert.assertNull(siteInfo.getType());
    }
}
