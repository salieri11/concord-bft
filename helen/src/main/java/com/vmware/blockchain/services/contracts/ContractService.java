/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.contracts;

import java.util.List;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.common.ConflictException;
import com.vmware.blockchain.common.ErrorCodeType;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.dao.GenericDao;

/**
 * A class which provides functions for doing contract management queries over the database.
 */
@Service
public class ContractService {
    private static Logger logger = LogManager.getLogger(ContractService.class);

    private GenericDao genericDao;

    @Autowired
    public ContractService(GenericDao genericDao) {
        this.genericDao = genericDao;
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
    public List<Contract> getContractVersion(String contractId, String versionName, UUID blockchain) {
        String json = JSONObject.toJSONString(ImmutableMap.of("name", contractId, "version_name", versionName));

        List<Contract> contracts = genericDao.getJsonByParentQuery(blockchain, json, Contract.class);
        return contracts;
    }

    /**
     * Save a new contract.  Make sure the
     */
    /**
     * Creates a new contractVersion with given details.
     *
     * @return True if a contract with given version was added successfully, False otherwise.
     * @throws ConflictException If there already exists another contract with same contractId and versionName,
     *         then this method throws this exception.
     */
    @Transactional
    public Contract addNewContractVersion(String contractId, String ownerAddress, String versionName, String address,
            String metaData, String byteCode, String sourceCode, UUID blockchain)
            throws ConflictException {
        if (getContractVersion(contractId, versionName, blockchain).isEmpty()) {
            Contract c = new Contract.ContractBuilder()
                    .name(contractId)
                    .versionName(versionName)
                    .sourcecode(sourceCode)
                    .bytecode(byteCode)
                    .metadata(metaData)
                    .address(address)
                    .owner(ownerAddress)
                    .blockchainId(blockchain).build();
            return genericDao.put(c, null);
        } else {
            throw new ConflictException(
                    "ContractVersion with id {0} and version {1} already exists", contractId, versionName);
        }
    }

    /**
     * Updates an existing contractVersion with given details.
     *
     * @return The updated contract
     * @throws ConflictException If there already exists another contract with same contractId and versionName,
     *         then this method throws this exception.
     */
    public Contract updateExistingContractVersion(String existingContractId, String existingVersionName,
            String contractId, String metaData, String sourceCode,
            UUID blockchain) {
        List<Contract> contracts = getContractVersion(existingContractId, existingVersionName, blockchain);
        if (contracts.isEmpty()) {
            throw new NotFoundException(ErrorCodeType.CONTRACT_VERSION_NOT_FOUND,
                                        existingContractId, existingVersionName);
        }
        return
            genericDao.mergeWithRetry(contracts.get(0), Contract.class, c -> {
                c.setName(contractId);
                c.setSourcecode(sourceCode);
                c.setMetadata(metaData);
            });
    }

    /**
     * Retrieve a list from a given name.
     *
     * @param contractId Contract Id
     * @return The BriefInfo object
     * @throws NotFoundException This exception is thrown when a contract with given contractId is not found.
     */
    public List<Contract> listByName(String contractId, UUID blockchain) {
        String json = JSONObject.toJSONString(ImmutableMap.of("name", contractId));
        List<Contract> contracts = genericDao.getJsonByParentQuery(blockchain, json, Contract.class);
        return contracts;
    }

    /**
     * Retrieve a list from a given blockchain.
     *
     * @return list of contracts in this blockchain.
     */
    public List<Contract> list(UUID blockchain) {
        List<Contract> contracts = genericDao.getByParentId(blockchain, Contract.class);
        return contracts;
    }
}
