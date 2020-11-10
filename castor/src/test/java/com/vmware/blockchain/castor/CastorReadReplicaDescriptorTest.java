/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsIterableContainingInAnyOrder.containsInAnyOrder;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.mock.env.MockEnvironment;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;
import com.vmware.blockchain.castor.model.ProvisionDescriptorDescriptorModel;
import com.vmware.blockchain.castor.service.CastorDeploymentType;
import com.vmware.blockchain.castor.service.DeployerService;
import com.vmware.blockchain.castor.service.DescriptorService;
import com.vmware.blockchain.castor.service.ValidationError;
import com.vmware.blockchain.castor.service.ValidatorService;

import lombok.extern.log4j.Log4j2;

/**
 * Test descriptor functionality.
 */
@Log4j2
@ExtendWith(SpringExtension.class)
@ContextConfiguration(classes = CastorTestConfiguration.class)
public class CastorReadReplicaDescriptorTest {

    private static final String INFRASTRUCTURE_DESCRIPTOR =
            "classpath:descriptors/test01_infrastructure_descriptor.json";
    private static final String VALID_DEPLOYMENT_DESCRIPTOR =
            "classpath:descriptors/test01_read_replica_valid_deployment_descriptor.json";
    private static final String INVALID_DEPLOYMENT_DESCRIPTOR =
            "classpath:descriptors/test01_read_replica_invalid_deployment_descriptor.json";

    private InfrastructureDescriptorModel validInfra;
    private ProvisionDescriptorDescriptorModel validDeployment;

    @Autowired
    private MockEnvironment environment;

    @Autowired
    private ResourceLoader resourceLoader;

    @Autowired
    private DescriptorService descriptorService;

    @Autowired
    private ValidatorService validatorService;

    @Autowired
    private DeployerService deployerService;

    /**
     * Set up valid descriptors before each test method.
     */
    @BeforeEach
    public void init() {
        // Build the model afresh for each test
        validInfra = DescriptorTestUtills.buildInfraDescriptorModel();
        validDeployment = DescriptorTestUtills.buildReadOnlyReplicaDeploymentDescriptorModel();
    }

    @Test
    public void testValidReadonlyReplicaDescriptorSerDeser() throws IOException {
        Resource infraResource = resourceLoader.getResource(INFRASTRUCTURE_DESCRIPTOR);
        String infraLocation = infraResource.getFile().getAbsolutePath();
        InfrastructureDescriptorModel readInfra =
                descriptorService.readInfrastructureDescriptorSpec(infraLocation);
        assertEquals(validInfra, readInfra, "infra descriptor model mismatch");
        assertEquals(validInfra.getOrganization(), readInfra.getOrganization(),
                     "infra descriptors organization mismatch");
        assertEquals(validInfra.getZones(), readInfra.getZones(), "infra descriptors zones mismatch");

        Resource deploymentResource = resourceLoader.getResource(VALID_DEPLOYMENT_DESCRIPTOR);
        String deploymentLocation = deploymentResource.getFile().getAbsolutePath();
        ProvisionDescriptorDescriptorModel readDeployment =
                (ProvisionDescriptorDescriptorModel) descriptorService.readDeploymentDescriptorSpec(
                        CastorDeploymentType.PROVISION, deploymentLocation);
        assertEquals(validDeployment, readDeployment, "deployment descriptor model mismatch");
        assertEquals(validDeployment.getBlockchain(), readDeployment.getBlockchain(),
                     "deployment descriptor blochchain model mismatch");
        assertEquals(validDeployment.getClients(), readDeployment.getClients(),
                     "deployment descriptor client model mismatch");
        assertEquals(validDeployment.getReplicas(), readDeployment.getReplicas(),
                     "deployment descriptor blochchain model mismatch");
        assertEquals(validDeployment.getReadonlyReplicas(), readDeployment.getReadonlyReplicas(),
                     "deployment descriptor blochchain model mismatch");
        assertEquals(validDeployment.getReadonlyReplicaNodeSpec(), readDeployment.getReadonlyReplicaNodeSpec(),
                     "deployment descriptor blochchain model mismatch");

        readDeployment.getClients().forEach(c -> assertNull(c.getAuthUrlJwt(), "client jwt auth url mismatch"));

        // No errors for valid descriptors
        List<ValidationError> constraints = validatorService.validate(
                CastorDeploymentType.PROVISION, validInfra, validDeployment);
        assertEquals(0, constraints.size());
    }

    @Test
    public void testInvalidReadonlyReplicaDeploymentDescriptor() throws IOException {
        Set<String> expectedErrorCodes = new HashSet<>();
        expectedErrorCodes.add("deployment.roreplica.access.key.invalid");
        expectedErrorCodes.add("deployment.roreplica.secret.key.invalid");
        expectedErrorCodes.add("deployment.roreplica.url.invalid");
        expectedErrorCodes.add("deployment.roreplica.bucket.name.invalid");

        Resource deploymentResource = resourceLoader.getResource(INVALID_DEPLOYMENT_DESCRIPTOR);
        String deploymentLocation = deploymentResource.getFile().getAbsolutePath();
        ProvisionDescriptorDescriptorModel readInvalidDeployment =
                (ProvisionDescriptorDescriptorModel) descriptorService.readDeploymentDescriptorSpec(
                        CastorDeploymentType.PROVISION, deploymentLocation);
        List<ValidationError> errors = validatorService.validate(
                CastorDeploymentType.PROVISION, validInfra, readInvalidDeployment);
        Set<String> validationErrorCodes = errors.stream()
                .map(ValidationError::getErrorCode).collect(Collectors.toSet());
        assertThat(validationErrorCodes, containsInAnyOrder(expectedErrorCodes.toArray()));
    }
}
