/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain;

import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

/**
 * Configuration file for Helen.
 */
@Configuration
@EnableJpaRepositories(basePackages = "com.vmware.blockchain")
public class JpaConfig implements WebMvcConfigurer {}
