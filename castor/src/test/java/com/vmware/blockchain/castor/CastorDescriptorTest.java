/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsIterableContainingInAnyOrder.containsInAnyOrder;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.mock.env.MockEnvironment;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.castor.exception.CastorException;
import com.vmware.blockchain.castor.model.DeploymentDescriptorModel;
import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;
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
public class CastorDescriptorTest {

    private static final String INFRASTRUCTURE_DESCRIPTOR =
            "classpath:descriptors/test01_infrastructure_descriptor.json";
    private static final String VALID_DEPLOYMENT_DESCRIPTOR =
            "classpath:descriptors/test01_deployment_descriptor.json";

    private static final String INVALID_URL_INFRASTRUCTURE_DESCRIPTOR_1 =
            "classpath:descriptors/test01_invalid_url_infrastructure_descriptor_1.json";
    private static final String INVALID_INFRASTRUCTURE_DESCRIPTOR_2 =
            "classpath:descriptors/test01_invalid_infrastructure_descriptor_2.json";
    private static final String INVALID_DEPLOYMENT_DESCRIPTOR =
            "classpath:descriptors/test01_invalid_deployment_descriptor.json";

    private InfrastructureDescriptorModel validInfra;
    private DeploymentDescriptorModel validDeployment;

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

    @TempDir
    File outputDir;

    /**
     * Set up valid descriptors before each test method.
     */
    @BeforeEach
    public void init() {
        // Build the model afresh for each test
        validInfra = DescriptorTestUtills.buildInfraDescriptorModel();
        validDeployment = DescriptorTestUtills.buildDeploymentDescriptorModel();
    }

    @Test
    public void testValidDescriptorSerDeser() throws IOException {
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
        DeploymentDescriptorModel readDeployment =
                descriptorService.readDeploymentDescriptorSpec(deploymentLocation);
        assertEquals(validDeployment, readDeployment, "deployment descriptor model mismatch");
        assertEquals(validDeployment.getBlockchain(), readDeployment.getBlockchain(),
                     "deployment descriptor blochchain model mismatch");
        assertEquals(validDeployment.getClients(), readDeployment.getClients(),
                     "deployment descriptor client model mismatch");
        assertEquals(validDeployment.getCommitters(), readDeployment.getCommitters(),
                     "deployment descriptor blochchain model mismatch");

        readDeployment.getClients().forEach(c -> assertNull(c.getAuthUrlJwt(), "client jwt auth url mismatch"));

        // No errors for valid descriptors
        List<ValidationError> constraints = validatorService.validate(validInfra, validDeployment);
        assertEquals(0, constraints.size());
    }

    @Test
    public void testInValidDescriptorModel() throws IOException {
        Set<String> expectedErrorCodes = new HashSet<>();
        final List<String> originalCommitters = validDeployment.getCommitters();
        // Deployment spec committers zone missing from Infra spec
        List<String> unknownZones = List.of("unz-1", "unz-2", "unz-3");
        validDeployment.setCommitters(unknownZones);
        expectedErrorCodes.add("deployment.zones.not.present.in.infrastructure");
        List<ValidationError> errors = validatorService.validate(validInfra, validDeployment);
        assertEquals(1, errors.size());
        Set<String> validationErrorCodes = errors.stream()
                .map(ValidationError::getErrorCode).collect(Collectors.toSet());
        assertThat(validationErrorCodes, containsInAnyOrder(expectedErrorCodes.toArray()));
        // Restore committers
        validDeployment.setCommitters(originalCommitters);

        // Deployment spec clients zone missing from Infra spec
        List<DeploymentDescriptorModel.Client> clients = validDeployment.getClients();

        // Used later to restore clients to a valid state
        List<String> originalClientZones =
                clients.stream().map(DeploymentDescriptorModel.Client::getZoneName).collect(Collectors.toList());

        validDeployment.getClients().forEach(c -> c.setZoneName("unz-1"));
        expectedErrorCodes.add("deployment.zones.not.present.in.infrastructure");
        errors = validatorService.validate(validInfra, validDeployment);
        assertEquals(1, errors.size());
        validationErrorCodes = errors.stream().map(ValidationError::getErrorCode).collect(Collectors.toSet());
        assertThat(validationErrorCodes, containsInAnyOrder(expectedErrorCodes.toArray()));

        // clear all required committers
        validDeployment.setCommitters(null);
        expectedErrorCodes.add("deployment.commiters.not.specified");
        errors = validatorService.validate(validInfra, validDeployment);
        assertEquals(2, errors.size());
        validationErrorCodes = errors.stream().map(ValidationError::getErrorCode).collect(Collectors.toSet());
        assertThat(validationErrorCodes, containsInAnyOrder(expectedErrorCodes.toArray()));

        // Restore committers, clients from the previous updates
        validDeployment.setCommitters(originalCommitters);
        for (int i = 0; i < clients.size(); ++i) {
            DeploymentDescriptorModel.Client c = clients.get(i);
            c.setZoneName(originalClientZones.get(i));
        }

        // Should have no validation errors since the model is good now
        errors = validatorService.validate(validInfra, validDeployment);
        assertEquals(0, errors.size());
    }

    @Test
    public void testInvalidDeploymentDescriptor() throws IOException {
        Set<String> expectedErrorCodes = new HashSet<>();
        // The invalid deployment descriptor contains zones not specified in the valid infra descriptor
        expectedErrorCodes.add("deployment.zones.not.present.in.infrastructure");
        // The blockchain type is UNKNOWN, which is invalid
        expectedErrorCodes.add("blockchain.type.invalid");
        Resource deploymentResource = resourceLoader.getResource(INVALID_DEPLOYMENT_DESCRIPTOR);
        String deploymentLocation = deploymentResource.getFile().getAbsolutePath();
        DeploymentDescriptorModel readInvalidDeployment =
                descriptorService.readDeploymentDescriptorSpec(deploymentLocation);
        List<ValidationError> errors = validatorService.validate(validInfra, readInvalidDeployment);
        Set<String> validationErrorCodes = errors.stream()
                .map(ValidationError::getErrorCode).collect(Collectors.toSet());
        assertThat(validationErrorCodes, containsInAnyOrder(expectedErrorCodes.toArray()));
    }

    @Test
    public void testInvalidUrlInfrastructureDescriptor() throws IOException {
        Resource infraResource = resourceLoader.getResource(INVALID_URL_INFRASTRUCTURE_DESCRIPTOR_1);
        String infraLocation = infraResource.getFile().getAbsolutePath();

        // The URL format is invalid, should throw exception during deserialization itself
        assertThrows(CastorException.class, () -> {
            descriptorService.readInfrastructureDescriptorSpec(infraLocation);
        });
    }

    @Test
    public void testInvalidInfrastructureDescriptor() throws IOException {
        Resource infraResource = resourceLoader.getResource(INVALID_INFRASTRUCTURE_DESCRIPTOR_2);
        String infraLocation = infraResource.getFile().getAbsolutePath();
        InfrastructureDescriptorModel readInvalidInfra =
                descriptorService.readInfrastructureDescriptorSpec(infraLocation);
        // This file has 4 errors:
        Set<String> expectedErrorCodes = new HashSet<>();
        expectedErrorCodes.add("vcenter.password.not.specified");
        expectedErrorCodes.add("network.name.not.specified");
        expectedErrorCodes.add("invalid.mincpu");
        expectedErrorCodes.add("invalid.minmemory");
        List<ValidationError> errors = validatorService.validate(readInvalidInfra, validDeployment);
        Set<String> validationErrorCodes = errors.stream()
                .map(ValidationError::getErrorCode).collect(Collectors.toSet());
        assertThat(validationErrorCodes, containsInAnyOrder(expectedErrorCodes.toArray()));
    }

    @Test
    public void testInvalidInfraDeployment() throws IOException {
        Resource invalidInfraResource = resourceLoader.getResource(INVALID_INFRASTRUCTURE_DESCRIPTOR_2);
        String invalidInfraLocation = invalidInfraResource.getFile().getAbsolutePath();
        environment.setProperty(DeployerService.INFRA_DESC_LOC_KEY, invalidInfraLocation);
        Resource deploymentResource = resourceLoader.getResource(VALID_DEPLOYMENT_DESCRIPTOR);
        String deploymentLocation = deploymentResource.getFile().getAbsolutePath();
        environment.setProperty(DeployerService.DEPL_DESC_LOC_KEY, deploymentLocation);

        File outputFile = new File(outputDir, "invalid.out");
        environment.setProperty(DeployerService.OUTPUT_DIR_LOC_KEY, outputFile.getAbsolutePath());
        // Should fail with errors because infra descriptor has invalid configuration
        assertThrows(CastorException.class, () -> {
            deployerService.start();
        });
    }
}
