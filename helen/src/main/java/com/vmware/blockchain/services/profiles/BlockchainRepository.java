/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.transaction.annotation.Transactional;

/**
 * JPA repository for blockchains.
 */
@Transactional("jpaTransaction")
public interface BlockchainRepository extends JpaRepository<Blockchain, UUID> {
    List<Blockchain> findAllByConsortium(Consortium consortium);

}
