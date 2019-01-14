/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import java.util.Optional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.common.HelenException;
import com.vmware.blockchain.services.profiles.User;
import com.vmware.blockchain.services.profiles.UserRepository;

/**
 * User details as required by Spring Security.
 */
@Service
public class HelenUserDetailsService implements UserDetailsService {
    static final Logger logger = LogManager.getLogger(HelenUserDetailsService.class);

    @Autowired
    private UserRepository userRepository;

    @Override
    @Cacheable("UserCache")
    public UserDetails loadUserByUsername(String email) throws UsernameNotFoundException {

        final Optional<User> user = userRepository.findUserByEmail(email);

        if (!user.isPresent()) {
            throw new HelenException("Invalid email/password", HttpStatus.UNAUTHORIZED);
        }

        User u = user.get();
        HelenUserDetails details = new HelenUserDetails(email, u.getPassword(), true, true, true, true, u.getRoles());
        details.setOrgId(u.getConsortium().getConsortiumId().toString());
        details.setAuthToken("");
        return details;
    }

}

