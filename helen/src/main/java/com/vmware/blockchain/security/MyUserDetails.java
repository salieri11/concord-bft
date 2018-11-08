/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import java.util.Optional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.services.profiles.User;
import com.vmware.blockchain.services.profiles.UserRepository;

@Service
public class MyUserDetails implements UserDetailsService {
    static final Logger logger = LogManager.getLogger(MyUserDetails.class);

    @Autowired
    private UserRepository userRepository;

    @Override
    @Cacheable("UserCache")
    public UserDetails loadUserByUsername(String email) throws UsernameNotFoundException {

        final Optional<User> user = userRepository.findUserByEmail(email);

        if (!user.isPresent()) {
            throw new UsernameNotFoundException("User '" + email + "' not found");
        }

        User u = user.get();

        return org.springframework.security.core.userdetails.User.withUsername(email).password(u.getPassword())
                .authorities(u.getRoles()).accountExpired(false).accountLocked(false).credentialsExpired(false)
                .disabled(false).build();
    }

}

