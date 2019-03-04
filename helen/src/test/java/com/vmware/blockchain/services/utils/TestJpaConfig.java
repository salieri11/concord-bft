/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.utils;

import javax.persistence.EntityManagerFactory;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.cache.CacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.transaction.PlatformTransactionManager;

import com.vmware.blockchain.StartupConfig;
import com.vmware.blockchain.WebSecurityConfig;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.security.AuthHelper;
import com.vmware.blockchain.security.JwtTokenProvider;

/**
 * JPA test configuration.
 */
@Configuration
@EntityScan(basePackages = {"com.vmware.blockchain.services.profiles", "com.vmware.blockchain.services.contracts"})
@EnableJpaRepositories(
        transactionManagerRef = "jpaTransaction",
        basePackages = {"com.vmware.blockchain.services.profiles", "com.vmware.blockchain.services.contracts"})
@EnableAutoConfiguration(exclude = {StartupConfig.class, WebSecurityConfig.class})
public class TestJpaConfig {
    @MockBean
    private PasswordEncoder passwordEncoder;

    @MockBean
    private ConnectionPoolManager cpManager;

    @MockBean
    private AuthHelper authHelper;

    @MockBean
    private JwtTokenProvider jwtTokenProvider;

    @MockBean
    private CacheManager cacheManager;

    /**
     * JPA Transaction Manager.
     */
    @Bean
    @Qualifier("jpaTransaction")
    public PlatformTransactionManager transactionManager(EntityManagerFactory emf) {
        JpaTransactionManager transactionManager = new JpaTransactionManager();
        transactionManager.setEntityManagerFactory(emf);

        return transactionManager;
    }

}
