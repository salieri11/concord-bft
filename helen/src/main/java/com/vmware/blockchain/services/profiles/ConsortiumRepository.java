/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import org.springframework.data.jpa.repository.JpaRepository;

/**
 * JPA repo for Consortiums.
 */
public interface ConsortiumRepository extends JpaRepository<Consortium, Long> {
}
