/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 */

package com.vmware.blockchain.services.profiles;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

public interface UserRepository extends JpaRepository<User, Long> {

   List<User> findUsersByConsortiumAndOrganization(Consortium c,
                                                   Organization o);
   
   Optional<User> findUserByEmail(String email);

   List<User> findUsersByConsortium(Consortium c);

   List<User> findUsersByOrganization(Organization o);
}
