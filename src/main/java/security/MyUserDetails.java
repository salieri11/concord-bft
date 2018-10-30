/**
 * Copyright 2018 VMware, all rights reserved.
 *
 */

package security;

import java.util.Optional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import services.profiles.User;
import services.profiles.UserRepository;

@Service
public class MyUserDetails implements UserDetailsService {
    static final Logger logger = LogManager.getLogger(MyUserDetails.class);

    @Autowired
    private UserRepository userRepository;

    @Autowired
    CacheManager cacheManager;

    @Override
    public UserDetails loadUserByUsername(String email) throws UsernameNotFoundException {
        Cache userCache = cacheManager.getCache("UserCache");
        // see if our user is cached
        UserDetails userDetails = userCache.get(email, UserDetails.class);
        if (userDetails != null) {
            return userDetails;
        }
            // Otherwise, look it up
        final Optional<User> user = userRepository.findUserByEmail(email);

        if (!user.isPresent()) {
            throw new UsernameNotFoundException("User '" + email + "' not found");
        }

        User u = user.get();

        userDetails = org.springframework.security.core.userdetails.User
                .withUsername(email)
                .password(u.getPassword())
                .authorities(u.getRoles())
                .accountExpired(false)
                .accountLocked(false)
                .credentialsExpired(false)
                .disabled(false)
                .build();
        userCache.put(email, userDetails);
        return userDetails;
    }

}

