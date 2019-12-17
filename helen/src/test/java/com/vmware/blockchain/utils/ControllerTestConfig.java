/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.utils;

import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.cache.CacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.security.crypto.password.PasswordEncoder;

import com.vmware.blockchain.BaseCacheHelper;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.DatabaseService;
import com.vmware.blockchain.common.csp.CspJwksSigningKeyResolver;
import com.vmware.blockchain.connections.ConcordConnectionPool;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.security.JwtTokenProvider;
import com.vmware.blockchain.security.ServiceContext;
import com.vmware.blockchain.security.VmbcTokenValidator;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.ethereum.EthDispatcher;
import com.vmware.blockchain.services.profiles.ConsortiumService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.KeystoreService;
import com.vmware.blockchain.services.profiles.OrganizationService;
import com.vmware.blockchain.services.profiles.ProfilesService;
import com.vmware.blockchain.services.profiles.UserService;
import com.vmware.blockchain.services.tasks.TaskService;

import io.grpc.ManagedChannel;

/**
 * Configuration for controller tests.
 */
@Configuration
public class ControllerTestConfig {
    @MockBean
    private UserService userService;

    @MockBean
    private ProfilesService prm;

    @MockBean
    private PasswordEncoder passwordEncoder;

    @MockBean
    private JwtTokenProvider jwtTokenProvider;

    @MockBean
    private ConcordConnectionPool connectionPool;

    @MockBean
    private KeystoreService keystoreService;

    @MockBean
    private DefaultProfiles profiles;

    @MockBean
    private BlockchainService blockchainService;

    @MockBean
    private ConsortiumService consortiumService;

    @MockBean
    private OrganizationService organizationService;

    @MockBean
    AuthHelper authHelper;

    @MockBean
    GenericDao genericDao;

    @MockBean
    ServiceContext serviceContext;

    @MockBean
    TaskService taskService;

    @MockBean
    @Qualifier("provisioningServerChannel")
    ManagedChannel channel;

    @MockBean
    EthDispatcher ethDispatcher;

    @MockBean
    ConnectionPoolManager connectionPoolManager;

    @MockBean
    BaseCacheHelper baseCacheHelper;

    @MockBean
    DatabaseService databaseService;

    @MockBean
    CacheManager cacheManager;

    @Bean
    @Primary
    VmbcTokenValidator cspAuthService() {
        return Mockito.mock(VmbcTokenValidator.class);
    }

    @Bean
    @Primary
    CspJwksSigningKeyResolver cspJwksSigningKeyResolver() {
        return Mockito.mock(CspJwksSigningKeyResolver.class);
    }

    @Bean
    @Primary
    OperationContext operationContext() {
        return Mockito.mock(OperationContext.class);
    }

}
