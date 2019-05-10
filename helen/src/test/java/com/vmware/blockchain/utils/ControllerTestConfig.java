/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.utils;

import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.crypto.password.PasswordEncoder;

import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.connections.ConcordConnectionPool;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.security.AuthHelper;
import com.vmware.blockchain.security.JwtTokenProvider;
import com.vmware.blockchain.security.ServiceContext;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.ethereum.EthDispatcher;
import com.vmware.blockchain.services.profiles.ConsortiumService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.KeystoreService;
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
    private ConcordProperties concordProperties;

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
    AuthHelper authHelper;

    @MockBean
    GenericDao genericDao;

    @MockBean
    ServiceContext serviceContext;

    @MockBean
    TaskService taskService;

    @MockBean
    ManagedChannel channel;

    @MockBean
    EthDispatcher ethDispatcher;

    @MockBean
    ConnectionPoolManager connectionPoolManager;

}
