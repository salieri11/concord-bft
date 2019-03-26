/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.contracts;

import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.transaction.annotation.Transactional;

/**
 * JPA repository to get Contracts.
 */
@Transactional("jpaTransaction")
public interface ContractRepository extends JpaRepository<Contract, UUID> {
    boolean existsByNameAndBlockchainId(String contractId, UUID blockchain);

    boolean existsByNameAndVersionNameAndBlockchainId(String contractName, String versionName, UUID blockchain);

    List<Contract> findByNameAndBlockchainId(String contractName, UUID blockchain);

    List<Contract> findByNameAndVersionNameAndBlockchainId(String contractId,
            String versionName, UUID blockchain);

    List<Contract> findByNameAndVersionNameAndBlockchainIdOrderBySeqDesc(String contractId,
            String versionName, UUID blockchain);

    List<Contract> findByNameAndBlockchainIdOrderBySeqDesc(String contractName, UUID blockchain);

    List<Contract> findAllByBlockchainIdOrderBySeqDesc(UUID blockchain);

    List<VersionProjection> findProjectionByNameAndBlockchainIdOrderBySeqDesc(String contractId, UUID blockchain);

    List<BriefContractProjection> findDistinctBriefByBlockchainIdOrderBySeqDesc(UUID blockchain);
}
