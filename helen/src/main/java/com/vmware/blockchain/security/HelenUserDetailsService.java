/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.common.UnauthorizedException;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.profiles.Organization;
import com.vmware.blockchain.services.profiles.OrganizationService;
import com.vmware.blockchain.services.profiles.User;
import com.vmware.blockchain.services.profiles.UserService;

/**
 * User details as required by Spring Security.
 */
@Service
public class HelenUserDetailsService implements UserDetailsService {
    static final Logger logger = LogManager.getLogger(HelenUserDetailsService.class);

    @Autowired
    private UserService userService;

    @Autowired
    private BlockchainService blockchainService;

    @Autowired
    private OrganizationService organizationService;

    @Override
    @Cacheable("UserCache")
    public UserDetails loadUserByUsername(String email) throws UsernameNotFoundException {

        final User u = userService.getByEmail(email);

        try {
            HelenUserDetails details =
                    new HelenUserDetails(u.getId(), email, u.getPassword(), u.getServiceRoles());
            Organization o = userService.getDefaultOrganization(u);
            details.setOrgId(o.getId());
            details.setAuthToken("");
            List<UUID> consortiums =
                    organizationService.getConsortiums(o.getId()).stream()
                            .map(c -> c.getId()).collect(Collectors.toList());
            details.setAccessConsortiums(consortiums);
            details.setUpdateConsortiums(consortiums);
            List<UUID> ids =
                    organizationService.getConsortiums(o.getId()).stream()
                            .map(blockchainService::listByConsortium)
                            .flatMap(c -> c.stream())
                            .map(b -> b.getId()).distinct().collect(Collectors.toList());
            details.setAccessChains(ids);
            details.setUpdateChains(ids);
            return details;
        } catch (NotFoundException e) {
            throw new UnauthorizedException(ErrorCode.BAD_LOGIN_REQUEST);
        }
    }

}

