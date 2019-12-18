/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import java.util.List;
import java.util.UUID;

import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.cache.CacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.core.annotation.Order;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.http.SessionCreationPolicy;

import com.vmware.blockchain.BaseCacheHelper;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.common.DatabaseService;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.services.ethereum.EthDispatcher;
import com.vmware.blockchain.services.profiles.Roles;
import com.vmware.blockchain.services.tasks.TaskService;

import io.grpc.ManagedChannel;

/**
 * MVC tests security configuration.
 */
@Configuration
@EnableWebSecurity
@EnableGlobalMethodSecurity(prePostEnabled = true)
public class MvcTestSecurityConfig extends WebSecurityConfigurerAdapter {

    protected void configure(HttpSecurity http) throws Exception {
        http.csrf().disable().sessionManagement().sessionCreationPolicy(SessionCreationPolicy.STATELESS).and()
                // Session fixation is a problem with the ui due to concurrency
                .sessionManagement().sessionFixation().none().and()
                .authorizeRequests()
                .antMatchers("/api/auth/login", "/api/auth/token", "/api/agreements/1", "/", "/assets/**").permitAll()
                .antMatchers("/api/oauth/login", "/api/oauth/oauth").permitAll()
                .anyRequest()
                .authenticated().and().exceptionHandling()
                .and().anonymous().and().httpBasic();
    }

    @Bean
    @Primary
    AuthHelper authHelper() {
        return new AuthHelper();
    }

    /**
     * Utility funtion to create an auth contrext for testing.
     */
    public static  AuthenticationContext createContext(String userName, UUID orgId, List<Roles> perms,
                                                       List<UUID> cons, List<UUID> chains, String authToken) {
        return createContext(userName, orgId, perms, cons, cons, chains, chains, authToken);
    }


    /**
     * Create a new authentication context to be used in MockMvc calls.
     * @param userName      User name
     * @param orgId         Organization
     * @param perms         Permissions of this user
     * @param cons          Accessable consortium IDs
     * @param updateCons    Updatable consoritium IDs
     * @param chains        Accessable blockchains IDs
     * @param updateChains  Updateable blockchain IDs
     * @param authToken     Auth token.  Actaully not used much.
     * @return              An auth context that can be passed in the invokation.
     */
    public static  AuthenticationContext createContext(String userName, UUID orgId, List<Roles> perms,
                                                       List<UUID> cons, List<UUID> updateCons,
                                                       List<UUID> chains, List<UUID> updateChains,
                                                       String authToken) {
        HelenUserDetails details = new HelenUserDetails(UUID.randomUUID(), userName, "", perms);
        details.setOrgId(orgId);
        details.setAuthToken(authToken);
        details.setAccessConsortiums(cons);
        details.setUpdateConsortiums(updateCons);
        details.setAccessChains(chains);
        details.setUpdateChains(updateChains);
        return new AuthenticationContext(details, perms);
    }


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
    @Order(99)
    public OperationContext operationContext() {
        return Mockito.mock(OperationContext.class);
    }

}
