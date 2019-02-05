/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

/**
 * JPA Keystores repo.  Empty for now.
 */
public interface KeystoreRepository extends JpaRepository<Keystore, String> {

    List<Keystore> findKeystoresByUser(User u);
}
