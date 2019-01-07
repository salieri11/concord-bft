/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;

/**
 * JPA repo for Consortiums.
 */
public interface ConsortiumRepository extends JpaRepository<Consortium, UUID> {
}
