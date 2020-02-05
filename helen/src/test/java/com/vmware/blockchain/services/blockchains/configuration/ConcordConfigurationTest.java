/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.configuration;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.UUID;

import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.services.configuration.ConcordConfiguration;
import com.vmware.blockchain.services.profiles.Organization;
import com.vmware.blockchain.services.profiles.OrganizationService;

/**
 * Tests for the concord config provider.
 */
@ExtendWith(SpringExtension.class)
public class ConcordConfigurationTest {

    ConcordConfiguration concordConfiguration;

    @MockBean
    OrganizationService organizationService;

    @MockBean
    AuthHelper authHelper;

    private Organization organization = mock(Organization.class);

    private String dockerImageTag = "imageTag1";
    private String dockerImageTag2 = "imageTag2";

    /**
     * Initialize various mocks.
     */
    @BeforeEach
    public void init() {
        concordConfiguration = new ConcordConfiguration(authHelper, organizationService, "1", "1", dockerImageTag);

        UUID orgId = UUID.randomUUID();
        when(authHelper.getOrganizationId()).thenReturn(orgId);
        when(organizationService.get(orgId)).thenReturn(organization);
    }

    @Test
    void testGetComponentsByBlockchainType() {

        var output = concordConfiguration.getComponentsByBlockchainType(ConcordModelSpecification.BlockchainType.DAML);
        Assert.assertEquals(9, output.size());
        output.stream().forEach(k -> Assert.assertTrue(k.getName().endsWith(dockerImageTag)));

        output = concordConfiguration.getComponentsByBlockchainType(ConcordModelSpecification.BlockchainType.ETHEREUM);
        Assert.assertEquals(7, output.size());
        output.stream().forEach(k -> Assert.assertTrue(k.getName().endsWith(dockerImageTag)));
    }


    @Test
    void testGetComponentsByBlockchainTypeByOrgOverride() {
        when(organization.getOrganizationProperties())
                .thenReturn(ImmutableMap.of(Constants.ORG_DOCKER_IMAGE_OVERRIDE, dockerImageTag2));
        var output = concordConfiguration.getComponentsByBlockchainType(ConcordModelSpecification.BlockchainType.DAML);
        Assert.assertEquals(9, output.size());
        output.stream().forEach(k -> Assert.assertTrue(k.getName().endsWith(dockerImageTag2)));

        output = concordConfiguration.getComponentsByBlockchainType(ConcordModelSpecification.BlockchainType.ETHEREUM);
        Assert.assertEquals(7, output.size());
        output.stream().forEach(k -> Assert.assertTrue(k.getName().endsWith(dockerImageTag2)));
    }
}
