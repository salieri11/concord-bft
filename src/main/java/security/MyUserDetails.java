/**
 * Copyright 2018 VMware, all rights reserved.
 *
 */

package security;

import services.profiles.User;
import services.profiles.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
public class MyUserDetails implements UserDetailsService {

  @Autowired
  private UserRepository userRepository;

  @Override
  public UserDetails loadUserByUsername(String email) throws UsernameNotFoundException {
    final Optional<User> user = userRepository.findUserByEmail(email);

    if (!user.isPresent()) {
      throw new UsernameNotFoundException("User '" + email + "' not found");
    }

    User u = user.get();

    return org.springframework.security.core.userdetails.User//
            .withUsername(email)//
            .password(u.getPassword())//
            .authorities(u.getRoles())//
            .accountExpired(false)//
            .accountLocked(false)//
            .credentialsExpired(false)//
            .disabled(false)//
            .build();
  }

}

