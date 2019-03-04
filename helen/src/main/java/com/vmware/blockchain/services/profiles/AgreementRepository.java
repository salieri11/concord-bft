/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.transaction.annotation.Transactional;

/**
 * JPA Agreements repo.  Empty for now.
 */
@Transactional("jpaTransaction")
public interface AgreementRepository extends JpaRepository<Agreement, Long> {

}
