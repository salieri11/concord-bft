/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import org.springframework.data.jpa.repository.JpaRepository;

/**
 * JPA Agreements repo.  Empty for now.
 */
public interface AgreementRepository extends JpaRepository<Agreement, Long> {

}
