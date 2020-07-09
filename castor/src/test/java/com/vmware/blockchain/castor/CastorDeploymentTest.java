/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.io.IOException;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.google.gson.FieldNamingPolicy;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.vmware.blockchain.castor.model.DeploymentDescriptorModel;
import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;
import com.vmware.blockchain.castor.service.DescriptorService;

import lombok.extern.log4j.Log4j2;

/**
 * Test descriptor functionality.
 */
@Log4j2
@ExtendWith(SpringExtension.class)
@ContextConfiguration(classes = CastorTestConfiguration.class)
public class CastorDeploymentTest {

    private static final String INFRASTRUCTURE_DESCRIPTOR =
            "classpath:descriptors/test01_infrastructure_descriptor.json";
    private static final String DEPLOYMENT_DESCRIPTOR =
            "classpath:descriptors/test01_deployment_descriptor.json";

    @Autowired
    ResourceLoader resourceLoader;

    @Autowired
    DescriptorService descriptorService;

    private static Gson gson = new GsonBuilder()
            .setPrettyPrinting()
            .setFieldNamingPolicy(FieldNamingPolicy.IDENTITY).create();

    @Test
    public void testDescriptorSerDeser() throws IOException {
        Resource infraResource = resourceLoader.getResource(INFRASTRUCTURE_DESCRIPTOR);
        String infraLocation = infraResource.getFile().getAbsolutePath();
        InfrastructureDescriptorModel infra = DescriptorTestUtills.buildInfraDescriptorModel();
        InfrastructureDescriptorModel readInfra =
                descriptorService.readInfrastructureDescriptorSpec(infraLocation);
        assertEquals(infra, readInfra, "infra descriptor model mismatch");
        assertEquals(infra.getOrganization(), readInfra.getOrganization(), "infra descriptors organization mismatch");
        assertEquals(infra.getZones(), readInfra.getZones(), "infra descriptors zones mismatch");

        Resource deploymentResource = resourceLoader.getResource(DEPLOYMENT_DESCRIPTOR);
        String deploymentLocation = deploymentResource.getFile().getAbsolutePath();
        DeploymentDescriptorModel deployment = DescriptorTestUtills.buildDeploymentDescriptorModel();
        DeploymentDescriptorModel readDeployment =
                descriptorService.readDeploymentDescriptorSpec(deploymentLocation);
        assertEquals(deployment, readDeployment, "deployment descriptor model mismatch");
        assertEquals(deployment.getBlockchain(), readDeployment.getBlockchain(),
                     "deployment descriptor blochchain model mismatch");
        assertEquals(deployment.getClients(), readDeployment.getClients(),
                     "deployment descriptor client model mismatch");
        assertEquals(deployment.getCommitters(), readDeployment.getCommitters(),
                     "deployment descriptor blochchain model mismatch");

        readDeployment.getClients().forEach(c -> assertNull(c.getAuthUrlJwt(), "client jwt auth url mismatch"));
    }
}
