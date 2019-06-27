/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;

import com.vmware.blockchain.MvcConfig;


/**
 * Tests for the consortium controller.
 */
@ExtendWith(SpringExtension.class)
@WebMvcTest(controllers = {ConsortiumController.class })
@ContextConfiguration(classes = MvcConfig.class)
@ComponentScan(basePackageClasses = {OrganizationContoller.class })
public class ConsortiumControllerTests {

    private MockMvc mockMvc;

}
