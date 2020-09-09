/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsIterableContainingInAnyOrder.containsInAnyOrder;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.InputStream;
import java.util.List;

import org.everit.json.schema.Schema;
import org.everit.json.schema.ValidationException;
import org.everit.json.schema.loader.SchemaLoader;
import org.json.JSONObject;
import org.json.JSONTokener;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import lombok.extern.log4j.Log4j2;

@SuppressWarnings({"checkstyle:JavadocType", "checkstyle:JavadocMethod"})
@Log4j2
@ExtendWith(SpringExtension.class)
public class JsonSchemaTest {

    private static final String INFRA_DESC_SCHEMA = "/infrastructure-descriptor-v1.schema";
    private static final String DEPLOYMENT_DESC_SCHEMA = "/deployment-descriptor-v1.schema";

    private static final String VALID_INFRA_01_MODEL = "/descriptors/test01_infrastructure_descriptor.json";
    private static final String INVALID_INFRA_MODEL_01 = "/descriptors/test01_invalid_infrastructure_descriptor_2.json";
    private static final String INVALID_INFRA_MODEL_02 = "/descriptors/test02_invalid_infrastructure_descriptor.json";

    private static final String VALID_DEPLOYMENT_01_MODEL = "/descriptors/test01_deployment_descriptor.json";
    private static final String INVALID_DEPLOYMENT_01_MODEL = "/descriptors/test01_invalid_deployment_descriptor.json";
    private static final String INVALID_DEPLOYMENT_02_MODEL = "/descriptors/test02_invalid_deployment_descriptor.json";

    private static Schema infraDescriptorSchema;
    private static Schema deploymentDescriptorSchema;

    @BeforeAll
    public static void init() {
        InputStream infraSchemaResource = JsonSchemaTest.class.getResourceAsStream(INFRA_DESC_SCHEMA);
        JSONTokener infraSchemaTokener = new JSONTokener(infraSchemaResource);
        JSONObject infraSchemaObject = new JSONObject(infraSchemaTokener);
        infraDescriptorSchema = SchemaLoader.load(infraSchemaObject);

        InputStream deploymentSchemaResource = JsonSchemaTest.class.getResourceAsStream(DEPLOYMENT_DESC_SCHEMA);
        JSONTokener deploymentSchemaTokener = new JSONTokener(deploymentSchemaResource);
        JSONObject deploymentSchemaObject = new JSONObject(deploymentSchemaTokener);
        deploymentDescriptorSchema = SchemaLoader.load(deploymentSchemaObject);
    }

    @Test
    public void testValidInfraJsonSchema() {
        InputStream validInfraModelResource = getClass().getResourceAsStream(VALID_INFRA_01_MODEL);
        JSONTokener validInfraModelTokener = new JSONTokener(validInfraModelResource);
        JSONObject validInfraModel = new JSONObject(validInfraModelTokener);

        // Valid infra model
        assertDoesNotThrow(() -> infraDescriptorSchema.validate(validInfraModel));
    }

    @Test
    public void testInvalidInfraJsonSchema01() {
        InputStream infraModelResource = getClass().getResourceAsStream(INVALID_INFRA_MODEL_01);
        JSONTokener infraModelTokener = new JSONTokener(infraModelResource);
        JSONObject infraModel = new JSONObject(infraModelTokener);

        // Invalid infra model
        ValidationException validationException =
                assertThrows(
                    ValidationException.class,
                    () -> infraDescriptorSchema.validate(infraModel),
                    "Errors should have been caught");

        String[] expectedMessages = new String[]{
            "#/organization/cpuCount: -4 is not greater or equal to 0",
            "#/zones/0/vCenter: required key [password] not found",
            "#/zones/0/network: required key [name] not found"
        };

        List<String> actualMessages = validationException.getAllMessages();
        assertEquals(3, actualMessages.size(), "Expected validation count mismatch");
        assertThat(actualMessages, containsInAnyOrder(expectedMessages));
    }

    @Test
    public void testInvalidInfraJsonSchema02() {
        InputStream infraModelResource = getClass().getResourceAsStream(INVALID_INFRA_MODEL_02);
        JSONTokener infraModelTokener = new JSONTokener(infraModelResource);
        JSONObject infraModel = new JSONObject(infraModelTokener);

        // Invalid infra model
        ValidationException validationException =
            assertThrows(
                ValidationException.class,
                () -> infraDescriptorSchema.validate(infraModel),
                "Errors should have been caught");

        String[] expectedMessages = new String[]{
            "#/zones/0/vCenter: required key [storage] not found",
            "#/zones/0/outboundProxy: required key [httpPort] not found",
            "#/zones/0/outboundProxy: required key [httpsHost] not found",
            "#/zones/0/logManagement/0/port: -9999 is not greater or equal to 0",
            "#/zones/0/logManagement/0/type: INVALID_LOG_MANAGEMENT is not a valid enum value",
        };

        List<String> actualMessages = validationException.getAllMessages();
        actualMessages.forEach(System.out::println);
        assertEquals(5, actualMessages.size(), "Expected validation count mismatch");
        assertThat(actualMessages, containsInAnyOrder(expectedMessages));
    }

    @Test
    public void testValidDeploymentJsonSchema() {
        InputStream validDeploymentModelResource = getClass().getResourceAsStream(VALID_DEPLOYMENT_01_MODEL);
        JSONTokener validDeploymentModelTokener = new JSONTokener(validDeploymentModelResource);
        JSONObject validDeploymentModel = new JSONObject(validDeploymentModelTokener);

        // Valid deployment model
        assertDoesNotThrow(() -> deploymentDescriptorSchema.validate(validDeploymentModel));
    }

    @Test
    public void testInvalidDeploymentJsonSchema01() {
        InputStream invalidDeploymentModelResource = getClass().getResourceAsStream(INVALID_DEPLOYMENT_01_MODEL);
        JSONTokener invalidDeploymentModelTokener = new JSONTokener(invalidDeploymentModelResource);
        JSONObject invalidDeploymentModel = new JSONObject(invalidDeploymentModelTokener);

        // Invalid deployment model
        ValidationException validationException = assertThrows(ValidationException.class, () -> {
            deploymentDescriptorSchema.validate(invalidDeploymentModel);
        });

        String[] expectedMessages = new String[]{
            "#/blockchain/bockchainType: INVALID is not a valid enum value"
        };

        List<String> actualMessages = validationException.getAllMessages();
        assertEquals(1, actualMessages.size(), "Expected validation count mismatch");
        assertThat(actualMessages, containsInAnyOrder(expectedMessages));
    }

    @Test
    public void testInvalidDeploymentJsonSchema02() {
        InputStream invalidDeploymentModelResource = getClass().getResourceAsStream(INVALID_DEPLOYMENT_02_MODEL);
        JSONTokener invalidDeploymentModelTokener = new JSONTokener(invalidDeploymentModelResource);
        JSONObject invalidDeploymentModel = new JSONObject(invalidDeploymentModelTokener);

        // Invalid deployment model
        ValidationException validationException = assertThrows(ValidationException.class, () -> {
            deploymentDescriptorSchema.validate(invalidDeploymentModel);
        });

        String[] expectedMessages = new String[]{
            "#/clients/0: required key [zoneName] not found",
            "#/blockchain: required key [consortiumName] not found",
            "#/committers/1/providedIp: [127374948] is not a valid ipv4 address"
        };

        List<String> actualMessages = validationException.getAllMessages();
        assertEquals(3, actualMessages.size(), "Expected validation count mismatch");
        assertThat(actualMessages, containsInAnyOrder(expectedMessages));
    }
}
