/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.powermock.reflect.Whitebox;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.mock.env.MockEnvironment;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.castor.exception.CastorException;
import com.vmware.blockchain.castor.exception.ErrorCode;
import com.vmware.blockchain.castor.service.DeployerService;

import lombok.extern.log4j.Log4j2;

/**
 * Test descriptor functionality.
 */
@Log4j2
@ExtendWith(SpringExtension.class)
@ContextConfiguration(classes = CastorTestConfiguration.class)
public class CastorOutputFileTest {

    private static final String INFRASTRUCTURE_DESCRIPTOR =
            "classpath:descriptors/test01_infrastructure_descriptor.json";
    private static final String VALID_DEPLOYMENT_DESCRIPTOR =
            "classpath:descriptors/test01_deployment_descriptor.json";

    @Autowired
    private ResourceLoader resourceLoader;

    @Autowired
    private DeployerService deployerService;

    @Autowired
    private MessageSource messageSource;

    /*
     * This is in a separate file because the environment needs to be cleared of the
     * DeployerService.OUTPUT_DIR_LOC_KEY property which is set/used in other tests.
     * This tests that the absence of this property will throw an exception.
     */
    @Test
    public void testMissingOutputFile() throws IOException {
        Resource infraResource = resourceLoader.getResource(INFRASTRUCTURE_DESCRIPTOR);
        String infraLocation = infraResource.getFile().getAbsolutePath();
        Resource deploymentResource = resourceLoader.getResource(VALID_DEPLOYMENT_DESCRIPTOR);
        String deploymentLocation = deploymentResource.getFile().getAbsolutePath();
        MockEnvironment mockEnvironment = new MockEnvironment()
                .withProperty(DeployerService.INFRA_DESC_LOC_KEY, infraLocation)
                .withProperty(DeployerService.DEPL_DESC_LOC_KEY, deploymentLocation);
        // Should fail with errors because the output file property is missing
        Whitebox.setInternalState(deployerService, "environment", mockEnvironment);
        CastorException exception = assertThrows(CastorException.class, () -> {
            deployerService.start();
        });
        String exceptionMessage = exception.getMessage();
        String expectedErrorMessage =
                ErrorCode.OUTPUT_DIR_PROPERTY_NOT_SET.replace("{0}", DeployerService.OUTPUT_DIR_LOC_KEY);
        assertEquals(expectedErrorMessage, exceptionMessage);
    }
}
