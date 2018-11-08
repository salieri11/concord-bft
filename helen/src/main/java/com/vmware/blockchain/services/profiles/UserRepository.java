/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

/**
 * JPA Repository for Users.
 */
public interface UserRepository extends JpaRepository<User, Long> {

    List<User> findUsersByConsortiumAndOrganization(Consortium c, Organization o);

    Optional<User> findUserByEmail(String email);

    List<User> findUsersByConsortium(Consortium c);

    List<User> findUsersByOrganization(Organization o);
}
