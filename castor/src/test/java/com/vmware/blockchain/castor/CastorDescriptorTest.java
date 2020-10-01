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
import org.powermock.reflect.Whitebox;
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

    private static final String ADV_INFRASTRUCTURE_DESCRIPTOR =
            "classpath:descriptors/test01_infrastructure_descriptor_with_advanced_features.json";

    private static final String INVALID_URL_INFRASTRUCTURE_DESCRIPTOR_1 =
            "classpath:descriptors/test01_invalid_url_infrastructure_descriptor_1.json";
    private static final String INVALID_INFRASTRUCTURE_DESCRIPTOR_2 =
            "classpath:descriptors/test01_invalid_infrastructure_descriptor_2.json";
    private static final String INVALID_DEPLOYMENT_DESCRIPTOR =
            "classpath:descriptors/test01_invalid_deployment_descriptor.json";
    // Client group validation
    private static final String VALID_CLIENT_GROUPS_DEPLOYMENT_DESCRIPTOR_1 =
            "classpath:descriptors/test01_valid_client_groups_deployment_descriptor.json";
    private static final String VALID_CLIENT_GROUPS_DEPLOYMENT_DESCRIPTOR_2 =
            "classpath:descriptors/test02_valid_client_groups_deployment_descriptor.json";
    private static final String INVALID_TOO_MANY_CLIENT_GROUPS_DEPLOYMENT_DESCRIPTOR =
            "classpath:descriptors/test_invalid_too_many_client_groups_deployment_descriptor.json";
    private static final String INVALID_TOO_MANY_CLIENTS_DEPLOYMENT_DESCRIPTOR =
            "classpath:descriptors/test_invalid_too_many_clients_deployment_descriptor.json";
    private static final String INVALID_TOO_MANY_CLIENTS_IN_GROUP_DEPLOYMENT_DESCRIPTOR =
            "classpath:descriptors/test_invalid_too_many_clients_in_group_deployment_descriptor.json";
    private static final String INFRASTRUCTURE_DESCRIPTOR_2 =
            "classpath:descriptors/test02_infrastructure_descriptor.json";

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
    public void testValidAdvancedFeaturesDescriptorSerDeser() throws IOException {
        Resource infraResource = resourceLoader.getResource(ADV_INFRASTRUCTURE_DESCRIPTOR);
        String infraLocation = infraResource.getFile().getAbsolutePath();
        InfrastructureDescriptorModel readInfra =
                descriptorService.readInfrastructureDescriptorSpec(infraLocation);
        assertEquals(validInfra, readInfra, "infra descriptor model mismatch");
        assertEquals(validInfra.getOrganization(), readInfra.getOrganization(),
                     "infra descriptors organization mismatch");
        assertEquals(1, readInfra.getOrganization().getAdvancedFeatures().size(),
                     "infra descriptors organization advanced features mismatch");
        assertEquals("False", readInfra.getOrganization().getAdvancedFeatures().get("SPLIT_CONFIG"),
                     "infra descriptors organization advanced features key/value mismatch");
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
    public void testInValidDescriptorModel() {
        Set<String> expectedErrorCodes = new HashSet<>();
        final List<DeploymentDescriptorModel.Committer> originalCommitters = validDeployment.getCommitters();
        // Deployment spec committers zone missing from Infra spec
        List<String> unknownZones = List.of("unz-1", "unz-2", "unz-3");
        List<DeploymentDescriptorModel.Committer> unknownZoneCommitters =
                unknownZones.stream().map(n -> DeploymentDescriptorModel.Committer.builder().zoneName(n).build())
                        .collect(
                                Collectors.toList());
        validDeployment.setCommitters(unknownZoneCommitters);
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
        assertThrows(CastorException.class, () -> descriptorService.readInfrastructureDescriptorSpec(infraLocation));
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
        assertThrows(CastorException.class, () -> deployerService.start());
    }

    @Test
    public void testValidProvidedIpAssignment() {
        InfrastructureDescriptorModel validIpsInfra = DescriptorTestUtills.buildInfraDescriptorModel();
        DeploymentDescriptorModel validIpsDeployment = DescriptorTestUtills.buildDeploymentDescriptorModel();
        // Add valid (unique, for all) IP addresses to clients and committers
        List<DeploymentDescriptorModel.Client> clients = validIpsDeployment.getClients();
        for (int i = 0; i < clients.size(); ++i) {
            DeploymentDescriptorModel.Client client = clients.get(i);
            client.setProvidedIp("10.11.12." + (i * 10));
        }
        List<DeploymentDescriptorModel.Committer> committers = validIpsDeployment.getCommitters();
        for (int i = 0; i < committers.size(); ++i) {
            DeploymentDescriptorModel.Committer committer = committers.get(i);
            committer.setProvidedIp("10.11.12." + ((clients.size() + i) * 10));
        }

        List<ValidationError> constraints = validatorService.validate(validIpsInfra, validIpsDeployment);
        assertEquals(0, constraints.size());
    }

    @Test
    public void testInvalidProvidedIpAssignment() {
        final InfrastructureDescriptorModel validIpsInfra = DescriptorTestUtills.buildInfraDescriptorModel();
        DeploymentDescriptorModel invalidIpsDeployment = DescriptorTestUtills.buildDeploymentDescriptorModel();

        // Check uniqueness for IPs: non-unique IPs for clients should error out
        Set<String> expectedErrorCodes = new HashSet<>();
        expectedErrorCodes.add("provided.ips.not.unique");
        List<DeploymentDescriptorModel.Client> clients = invalidIpsDeployment.getClients();
        clients.forEach(c -> c.setProvidedIp("1.1.1.1"));
        List<DeploymentDescriptorModel.Committer> committers = invalidIpsDeployment.getCommitters();
        committers.forEach(c -> c.setProvidedIp("2.2.2.2"));

        List<ValidationError> errors = validatorService.validate(validIpsInfra, invalidIpsDeployment);
        Set<String> validationErrorCodes = errors.stream()
                .map(ValidationError::getErrorCode).collect(Collectors.toSet());
        assertThat(validationErrorCodes, containsInAnyOrder(expectedErrorCodes.toArray()));

        // Add IPs only for certain clients
        for (int i = 0; i < clients.size(); i++) {
            DeploymentDescriptorModel.Client client = clients.get(i);
            if (i % 2 == 0) {
                client.setProvidedIp(null);
            } else {
                client.setProvidedIp("10.11.12." + (i * 10));
            }
        }
        // Add IPs only for certain clients
        committers = invalidIpsDeployment.getCommitters();
        for (int i = 0; i < committers.size(); i++) {
            DeploymentDescriptorModel.Committer committer = committers.get(i);
            if (i % 2 == 0) {
                committer.setProvidedIp("10.11.12." + ((clients.size() + i) * 10));
            } else {
                committer.setProvidedIp("         ");
            }
        }

        expectedErrorCodes.clear();
        expectedErrorCodes.add("not.all.ips.specified.for.deployment");

        errors = validatorService.validate(validIpsInfra, invalidIpsDeployment);
        validationErrorCodes = errors.stream()
                .map(ValidationError::getErrorCode).collect(Collectors.toSet());
        assertThat(validationErrorCodes, containsInAnyOrder(expectedErrorCodes.toArray()));


    }

    /**
     * Test case adding 11 client groups to DD.
     * @throws IOException IOException
     */
    @Test
    public void testInValidTooManyClientGroups() throws IOException {
        Set<String> expectedErrorCodes = new HashSet<>();
        // The invalid deployment descriptor contains too many client groups.
        expectedErrorCodes.add("deployment.too.many.client.groups");
        Resource deploymentResource = resourceLoader.getResource(INVALID_TOO_MANY_CLIENT_GROUPS_DEPLOYMENT_DESCRIPTOR);
        String deploymentLocation = deploymentResource.getFile().getAbsolutePath();
        DeploymentDescriptorModel readInvalidDeployment =
                descriptorService.readDeploymentDescriptorSpec(deploymentLocation);

        Resource infraResource = resourceLoader.getResource(INFRASTRUCTURE_DESCRIPTOR_2);
        String infraLocation = infraResource.getFile().getAbsolutePath();
        InfrastructureDescriptorModel readInfra =
                descriptorService.readInfrastructureDescriptorSpec(infraLocation);
        // Set the max client groups property.
        environment.setProperty(ValidatorService.DEPLOYMENT_MAX_CLIENT_GROUPS, "10");
        // For some reason, autowiring does not work for environment. Force it.
        Whitebox.setInternalState(validatorService, "environment", environment);

        List<ValidationError> errors = validatorService.validate(readInfra, readInvalidDeployment);
        Set<String> validationErrorCodes = errors.stream()
                .map(ValidationError::getErrorCode).collect(Collectors.toSet());
        assertThat(validationErrorCodes, containsInAnyOrder(expectedErrorCodes.toArray()));
    }

    /**
     * Test case adding 11 Clients to DD.
     * @throws IOException IOException
     */
    @Test
    public void testInvalidTooManyClients() throws IOException {
        Set<String> expectedErrorCodes = new HashSet<>();
        // The invalid deployment descriptor contains too many clients.
        expectedErrorCodes.add("deployment.invalid.client.count");
        Resource deploymentResource = resourceLoader.getResource(INVALID_TOO_MANY_CLIENTS_DEPLOYMENT_DESCRIPTOR);
        String deploymentLocation = deploymentResource.getFile().getAbsolutePath();
        DeploymentDescriptorModel readInvalidDeployment =
                descriptorService.readDeploymentDescriptorSpec(deploymentLocation);

        Resource infraResource = resourceLoader.getResource(INFRASTRUCTURE_DESCRIPTOR_2);
        String infraLocation = infraResource.getFile().getAbsolutePath();
        InfrastructureDescriptorModel readInfra =
                descriptorService.readInfrastructureDescriptorSpec(infraLocation);
        // Set the number of clients range property.
        environment.setProperty(ValidatorService.DEPLOYMENT_NUM_CLIENTS_RANGE_KEY, "{1,10}");
        // For some reason, autowiring does not work for environment. Force it.
        Whitebox.setInternalState(validatorService, "environment", environment);

        List<ValidationError> errors = validatorService.validate(readInfra, readInvalidDeployment);
        Set<String> validationErrorCodes = errors.stream()
                .map(ValidationError::getErrorCode).collect(Collectors.toSet());
        assertThat(validationErrorCodes, containsInAnyOrder(expectedErrorCodes.toArray()));
    }

    /**
     * Test case adding 7 Clients to a Group.
     * @throws IOException IOException
     */
    @Test
    public void testInvalidTooManyClientsInGroup() throws IOException {
        Set<String> expectedErrorCodes = new HashSet<>();
        // The invalid deployment descriptor contains too many clients in a group.
        expectedErrorCodes.add("deployment.too.many.clients.in.group");
        Resource deploymentResource = resourceLoader.getResource(
                INVALID_TOO_MANY_CLIENTS_IN_GROUP_DEPLOYMENT_DESCRIPTOR);
        String deploymentLocation = deploymentResource.getFile().getAbsolutePath();
        DeploymentDescriptorModel readInvalidDeployment =
                descriptorService.readDeploymentDescriptorSpec(deploymentLocation);
        Resource infraResource = resourceLoader.getResource(INFRASTRUCTURE_DESCRIPTOR_2);
        String infraLocation = infraResource.getFile().getAbsolutePath();
        InfrastructureDescriptorModel readInfra =
                descriptorService.readInfrastructureDescriptorSpec(infraLocation);

        // Set the max clients per group property.
        environment.setProperty(ValidatorService.DEPLOYMENT_MAX_CLIENTS_PER_GROUP, "6");
        // For some reason, autowiring does not work for environment. Force it.
        Whitebox.setInternalState(validatorService, "environment", environment);

        List<ValidationError> errors = validatorService.validate(readInfra, readInvalidDeployment);
        Set<String> validationErrorCodes = errors.stream()
                .map(ValidationError::getErrorCode).collect(Collectors.toSet());
        assertThat(validationErrorCodes, containsInAnyOrder(expectedErrorCodes.toArray()));
    }

    /**
     * Test case adding 3 Clients to 2 Groups.
     * @throws IOException IOException
     */
    @Test
    public void testValidClientGroups1() throws IOException {
        Resource deploymentResource = resourceLoader.getResource(VALID_CLIENT_GROUPS_DEPLOYMENT_DESCRIPTOR_1);
        String deploymentLocation = deploymentResource.getFile().getAbsolutePath();
        DeploymentDescriptorModel readInvalidDeployment =
                descriptorService.readDeploymentDescriptorSpec(deploymentLocation);
        Resource infraResource = resourceLoader.getResource(INFRASTRUCTURE_DESCRIPTOR_2);
        String infraLocation = infraResource.getFile().getAbsolutePath();
        InfrastructureDescriptorModel readInfra =
                descriptorService.readInfrastructureDescriptorSpec(infraLocation);

        List<ValidationError> errors = validatorService.validate(readInfra, readInvalidDeployment);
        Set<String> validationErrorCodes = errors.stream()
                .map(ValidationError::getErrorCode).collect(Collectors.toSet());
        assertEquals(0, validationErrorCodes.size());
    }

    /**
     * Test case adding 3 Clients to 3 Groups.
     * @throws IOException IOException
     */
    @Test
    public void testValidClientGroups2() throws IOException {
        Resource deploymentResource = resourceLoader.getResource(VALID_CLIENT_GROUPS_DEPLOYMENT_DESCRIPTOR_2);
        String deploymentLocation = deploymentResource.getFile().getAbsolutePath();
        DeploymentDescriptorModel readInvalidDeployment =
                descriptorService.readDeploymentDescriptorSpec(deploymentLocation);
        Resource infraResource = resourceLoader.getResource(INFRASTRUCTURE_DESCRIPTOR_2);
        String infraLocation = infraResource.getFile().getAbsolutePath();
        InfrastructureDescriptorModel readInfra =
                descriptorService.readInfrastructureDescriptorSpec(infraLocation);
        List<ValidationError> errors = validatorService.validate(readInfra, readInvalidDeployment);
        Set<String> validationErrorCodes = errors.stream()
                .map(ValidationError::getErrorCode).collect(Collectors.toSet());
        assertEquals(0, validationErrorCodes.size());
    }

}
