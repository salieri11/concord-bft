/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.transaction.annotation.Transactional;

/**
 * JPA Repository for Users.
 */
@Transactional("jpaTransaction")
public interface UserRepository extends JpaRepository<User, UUID> {

    List<User> findUsersByConsortiumAndOrganization(Consortium c, Organization o);

    Optional<User> findUserByEmail(String email);

    Optional<User> findUserByUserId(UUID id);

    List<User> findUsersByConsortium(Consortium c);

    List<User> findUsersByOrganization(Organization o);
}
