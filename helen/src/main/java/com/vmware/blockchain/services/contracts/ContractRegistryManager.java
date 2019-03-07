/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.contracts;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.common.ConflictException;
import com.vmware.blockchain.common.NotFoundException;

/**
 * A class which provides functions for doing contract management queries over the database.
 */
// TODO: Have all database queries and configs in separate DBconfig file.
@Component
public class ContractRegistryManager {
    private static Logger logger = LogManager.getLogger(ContractRegistryManager.class);

    private ContractRepository contractReopository;

    @Autowired
    public ContractRegistryManager(ContractRepository contractRepository) {
        this.contractReopository = contractRepository;
    }

    /**
     * A method to check if a contract with given contractId exists.
     *
     * @param contractId The `contractId` to check for
     * @return True if a contract with given contractId exists, False otherwise.
     */
    public boolean hasContract(String contractId, UUID blockchain) {
        return contractReopository.existsByNameAndBlockchainId(contractId, blockchain);
    }

    /**
     * Checks if a contract with given contractId and given versionName exists.
     *
     * @param contractId Contract
     * @param versionName Version
     * @return True if such a contract exists, False otherwise
     */
    public boolean hasContractVersion(String contractId, String versionName, UUID blockchain) {
        return contractReopository.existsByNameAndVersionNameAndBlockchainId(contractId, versionName, blockchain);
    }

    /**
     * Retrieves a particular version of a particular contract.
     *
     * @param contractId The contractId to look for
     * @param versionName The versionName to look for in contract with ID as contractId
     * @return A FullVersionInfo object which can be queries to get information about this contract version.
     * @throws NotFoundException In case contract was not found or some other database exception occurs then
     *         throws this exception.
     */
    public FullVersionInfo getContractVersion(String contractId, String versionName, UUID blockchain) {
        List<Contract> contracts = contractReopository
                .findByNameAndVersionNameAndBlockchainId(contractId, versionName, blockchain);

        if (!contracts.isEmpty()) {
            Contract c = contracts.get(0);
            ContractVersion cv = new ContractVersion(c.getName(),
                    c.getOwner(), c.getVersionName(),
                    c.getAddress(), c.getMetadata(),
                    c.getBytecode(), c.getSourcecode());
            return cv;
        } else {
            throw new NotFoundException("Contract with contract ID: {0} and version {1} not found", contractId,
                    versionName);
        }
    }

    /**
     * Creates a new contractVersion with given details.
     *
     * @return True if a contract with given version was added successfully, False otherwise.
     * @throws ConflictException If there already exists another contract with same contractId and versionName,
     *         then this method throws this exception.
     */
    public boolean addNewContractVersion(String contractId, String ownerAddress, String versionName, String address,
            String metaData, String byteCode, String sourceCode, UUID blockchain)
            throws ConflictException {
        try {
            Contract c = new Contract.ContractBuilder()
                    .name(contractId)
                    .versionName(versionName)
                    .sourcecode(sourceCode)
                    .bytecode(byteCode)
                    .metadata(metaData)
                    .address(address)
                    .owner(ownerAddress)
                    .blockchainId(blockchain).build();
            contractReopository.save(c);
        } catch (DataIntegrityViolationException e) {
            throw new ConflictException(
                    "ContractVersion with id {0} and version {1} already exists", contractId, versionName);
        }
        return true;
    }

    /**
     * Updates an existing contractVersion with given details.
     *
     * @return True if a contract with given version was updated successfully, False otherwise.
     * @throws ConflictException If there already exists another contract with same contractId and versionName,
     *         then this method throws this exception.
     */
    public boolean updateExistingContractVersion(String existingContractId, String existingVersionName,
            String contractId, String ownerAddress, String versionName, String metaData, String sourceCode,
            UUID blockchain) {
        {
            if (hasContractVersion(contractId, versionName, blockchain)) {
                throw new ConflictException(
                        "ContractVersion with id {0} and version {1} already exists", contractId, versionName);
            } else {
                List<Contract> contracts =
                        contractReopository.findByNameAndVersionNameAndBlockchainIdOrderBySeqDesc(
                                existingContractId, existingVersionName, blockchain);
                if (contracts.isEmpty()) {
                    throw new NotFoundException("No contract with id {0} and version {2}",
                            existingContractId, existingVersionName);
                }
                Contract c = contracts.get(0);
                c.setName(contractId);
                c.setVersionName(versionName);
                c.setOwner(ownerAddress);
                c.setSourcecode(sourceCode);
                c.setMetadata(metaData);
                contractReopository.save(c);
            }
        }
        return true;
    }

    /**
     * Retrieve a BriefContractInfo about a particular contract.
     *
     * @param contractId Contract Id
     * @return The BriefInfo object
     * @throws NotFoundException This exception is thrown when a contract with given contractId is not found.
     */
    public BriefContractInfo getBriefContractInfo(String contractId, UUID blockchain) {
        List<Contract> contracts = contractReopository.findByNameAndBlockchainIdOrderBySeqDesc(contractId, blockchain);
        if (contracts.isEmpty()) {
            throw new NotFoundException("No contract with id {0}", contractId);
        }
        Contract c = contracts.get(0);
        BriefContractInfo bcInfo = new ContractVersion.ContractVersionBuilder()
                .contractId(c.getName())
                .ownerAddress(c.getOwner())
                .build();
        return bcInfo;
    }

    /**
     * Returns a list of BriefContractInfo objects, containing 1 BriefContractInfo object for every unique contractId
     * present in the database.
     *
     * @return The List of BriefContractInfo objects.
     */
    public List<BriefContractInfo> getAllBriefContractInfo(UUID blockchain) {
        // TODO: It is possible that contract list gets very big, then it
        // doesn't make sense to send everything over REST. Modify REST API
        // to handle that.
        List<BriefContractInfo> contracts = new ArrayList<>();
        List<BriefContractProjection> info =
                contractReopository.findDistinctBriefByBlockchainIdOrderBySeqDesc(blockchain);
        for (BriefContractProjection b : info) {
            contracts.add(new ContractVersion.ContractVersionBuilder().contractId(b.getName())
                    .ownerAddress(b.getOwner()).build());
        }
        return contracts;
    }

    /**
     * Retrieves BriefVersionInfo of all available versions of a contract identified by contractId.
     *
     * @param contractId Contract Id
     * @return Returns a list of all BriefVersionInfo objects.
     */
    public List<BriefVersionInfo> getAllBriefVersionInfo(String contractId, UUID blockchain) {
        List<BriefVersionInfo> versions = new ArrayList<>();
        List<VersionProjection> contracts =
                contractReopository.findProjectionByNameAndBlockchainIdOrderBySeqDesc(contractId, blockchain);
        for (VersionProjection vp : contracts) {
            ContractVersion cv = new ContractVersion.ContractVersionBuilder()
                    .contractId(contractId)
                    .ownerAddress(vp.getOwner())
                    .address(vp.getAddress())
                    .metaData(vp.getMetadata())
                    .versionName(vp.getVersionName())
                    .build();
            versions.add(cv);
        }
        return versions;
    }
}
