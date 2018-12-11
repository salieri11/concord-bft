/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.contracts;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.common.DatabaseService;
import com.vmware.blockchain.common.ServiceUnavailableException;

/**
 * A class which provides functions for doing contract management queries over the database.
 */
// TODO: Have all database queries and configs in separate DBconfig file.
@Component
public class ContractRegistryManager {
    private static Logger logger = LogManager.getLogger(ContractRegistryManager.class);

    private static final String CONTRACTS_TABLE_NAME = "contracts";
    private static final String CONTRACT_ID_COLUMN_LABEL = "contract_id";
    private static final String CONTRACT_VERSION_COLUMN_LABEL = "version_name";
    private static final String CONTRACT_SOURCE_COLUMN_LABEL = "sourcecode";
    private static final String CONTRACT_BYTECODE_COLUMN_LABEL = "bytecode";
    private static final String CONTRACT_METADATA_COLUMN_LABEL = "metadata";
    private static final String CONTRACT_ADDRESS_COLUMN_LABEL = "address";
    private static final String CONTRACT_OWNER_COLUMN_LABEL = "owner";
    private static final String CONTRACT_SEQUENCE_NUMBER_LABEL = "sequence_number";
    private static final int MAX_ADDRESS_LEN = 66; // 32 bytes + '0x'

    // TODO: Later on we might feel the need to split these things into
    // multiple tables but as of now having everything in single table is easier.
    // sequence_number is of cockroach DB SERIAL type which ensures that every
    // insert gets a higher sequence number than previous ones.
    private String createNewTableQuery = "CREATE TABLE IF NOT EXISTS " + CONTRACTS_TABLE_NAME + "("
            + CONTRACT_ID_COLUMN_LABEL + " TEXT, " + CONTRACT_VERSION_COLUMN_LABEL + " TEXT, PRIMARY KEY ("
            + CONTRACT_ID_COLUMN_LABEL + "," + CONTRACT_VERSION_COLUMN_LABEL + "), " + CONTRACT_SOURCE_COLUMN_LABEL
            + " TEXT, " + CONTRACT_BYTECODE_COLUMN_LABEL + " TEXT, " + CONTRACT_METADATA_COLUMN_LABEL + " TEXT, "
            + CONTRACT_ADDRESS_COLUMN_LABEL + " char(" + MAX_ADDRESS_LEN + "), " + CONTRACT_OWNER_COLUMN_LABEL
            + " char(" + MAX_ADDRESS_LEN + "), " + CONTRACT_SEQUENCE_NUMBER_LABEL + " SERIAL)";

    private String insertNewVersionQuery = "INSERT into " + CONTRACTS_TABLE_NAME + " values (?, ?, ?, ?, ?, ?, ?)";

    private String updateExistingVersionQuery = "UPDATE " + CONTRACTS_TABLE_NAME + " SET (" + CONTRACT_ID_COLUMN_LABEL
            + ", " + CONTRACT_SOURCE_COLUMN_LABEL + ", " + CONTRACT_METADATA_COLUMN_LABEL + ", "
            + CONTRACT_OWNER_COLUMN_LABEL + ", " + CONTRACT_VERSION_COLUMN_LABEL + ") = (?, ?, ?, ?, ?) WHERE "
            + CONTRACT_ID_COLUMN_LABEL + " = ? AND " + CONTRACT_VERSION_COLUMN_LABEL + " = ?;";

    private String hasContractQuery = "SELECT " + CONTRACT_ID_COLUMN_LABEL + " from " + CONTRACTS_TABLE_NAME + " where "
            + CONTRACT_ID_COLUMN_LABEL + " = ?";

    private String hasVersionQuery = "SELECT " + CONTRACT_ID_COLUMN_LABEL + " from " + CONTRACTS_TABLE_NAME + " where "
            + CONTRACT_ID_COLUMN_LABEL + " = ? AND " + CONTRACT_VERSION_COLUMN_LABEL + " = ?";

    private String getVersionQuery = "SELECT * from " + CONTRACTS_TABLE_NAME + " where " + CONTRACT_ID_COLUMN_LABEL
            + " = ? AND " + CONTRACT_VERSION_COLUMN_LABEL + " = ?";

    private String getAllContractsShortQuery = "SELECT DISTINCT " + CONTRACT_ID_COLUMN_LABEL + ", "
            + CONTRACT_OWNER_COLUMN_LABEL + " from " + CONTRACTS_TABLE_NAME + " ORDER BY "
            + CONTRACT_SEQUENCE_NUMBER_LABEL + " DESC";

    // We need versions sorted in descending order of sequence_number column.
    private String getAllVersionsShortQuery = "SELECT " + CONTRACT_VERSION_COLUMN_LABEL + ", "
            + CONTRACT_METADATA_COLUMN_LABEL + ", " + CONTRACT_ADDRESS_COLUMN_LABEL + ", " + CONTRACT_OWNER_COLUMN_LABEL
            + " from " + CONTRACTS_TABLE_NAME + " where " + CONTRACT_ID_COLUMN_LABEL + " = ? ORDER BY "
            + CONTRACT_SEQUENCE_NUMBER_LABEL + " DESC";

    private String getContractOwnerQuery = "SELECT DISTINCT" + CONTRACT_OWNER_COLUMN_LABEL + " from "
            + CONTRACTS_TABLE_NAME + " where " + CONTRACT_ID_COLUMN_LABEL + " = ?";

    private String getContractInfoQuery = "SELECT " + CONTRACT_ID_COLUMN_LABEL + ", " + CONTRACT_OWNER_COLUMN_LABEL
            + " from " + CONTRACTS_TABLE_NAME + " where " + CONTRACT_ID_COLUMN_LABEL + " = ?";

    // TODO: we might have to write method which closes all these statement
    // objects. However, that will only have to be done when our servlet dies
    // which won't happen very frequently.
    private PreparedStatement insertNewVersionPstmt;
    private PreparedStatement updateExistingVersionPstmt;
    private PreparedStatement hasContractPstmt;
    private PreparedStatement hasVersionPstmt;
    private PreparedStatement getVersionPstmt;
    private PreparedStatement getAllContractsShortPstmt;
    private PreparedStatement getAllVersionsShortPstmt;
    private PreparedStatement getContractOwnerPstmt;
    private PreparedStatement getContractInfoPstmt;

    private ContractRegistryManager(@Autowired DatabaseService databaseService)
            throws SQLException, ServiceUnavailableException {

        Connection con = databaseService.getDatabaseConnection();
        con.createStatement().executeUpdate(createNewTableQuery);

        insertNewVersionPstmt = con.prepareStatement(insertNewVersionQuery);
        updateExistingVersionPstmt = con.prepareStatement(updateExistingVersionQuery);
        hasContractPstmt = con.prepareStatement(hasContractQuery);
        hasVersionPstmt = con.prepareStatement(hasVersionQuery);
        getVersionPstmt = con.prepareStatement(getVersionQuery);
        getAllContractsShortPstmt = con.prepareStatement(getAllContractsShortQuery);
        getAllVersionsShortPstmt = con.prepareStatement(getAllVersionsShortQuery);
        getContractOwnerPstmt = con.prepareStatement(getContractOwnerQuery);
        getContractInfoPstmt = con.prepareStatement(getContractInfoQuery);
    }

    /**
     * A method to check if a contract with given contractId exists.
     *
     * @param contractId The `contractId` to check for
     * @return True if a contract with given contractId exists, False otherwise.
     */
    public boolean hasContract(String contractId) {
        try {
            hasContractPstmt.setString(1, contractId);
            ResultSet rs = hasContractPstmt.executeQuery();
            return rs.isBeforeFirst();
        } catch (SQLException e) {
            logger.warn("Exception in hasContract", e);
        }
        return false;
    }

    /**
     * Checks if a contract with given contractId and given versionName exists.
     *
     * @param contractId Contract
     * @param versionName Version
     * @return True if such a contract exists, False otherwise
     */
    public boolean hasContractVersion(String contractId, String versionName) {
        try {
            hasVersionPstmt.setString(1, contractId);
            hasVersionPstmt.setString(2, versionName);
            ResultSet rs = hasVersionPstmt.executeQuery();
            return rs.isBeforeFirst();
        } catch (SQLException e) {
            logger.warn("Exception in hasContractVersion", e);
        }
        return false;
    }

    /**
     * Retrieves a particular version of a particular contract.
     *
     * @param contractId The contractId to look for
     * @param versionName The versionName to look for in contract with ID as contractId
     * @return A FullVersionInfo object which can be queries to get information about this contract version.
     * @throws ContractRetrievalException In case contract was not found or some other database exception occurs then
     *         throws this exception.
     */
    public FullVersionInfo getContractVersion(String contractId, String versionName) throws ContractRetrievalException {
        try {
            getVersionPstmt.setString(1, contractId);
            getVersionPstmt.setString(2, versionName);
            ResultSet rs = getVersionPstmt.executeQuery();

            if (rs.isBeforeFirst()) {
                rs.next();
                ContractVersion cv = new ContractVersion(rs.getString(CONTRACT_ID_COLUMN_LABEL),
                        rs.getString(CONTRACT_OWNER_COLUMN_LABEL), rs.getString(CONTRACT_VERSION_COLUMN_LABEL),
                        rs.getString(CONTRACT_ADDRESS_COLUMN_LABEL), rs.getString(CONTRACT_METADATA_COLUMN_LABEL),
                        rs.getString(CONTRACT_BYTECODE_COLUMN_LABEL), rs.getString(CONTRACT_SOURCE_COLUMN_LABEL));
                return cv;
            } else {
                throw new ContractRetrievalException("Contract with contract ID: " + contractId + " and version: "
                        + versionName + " not " + "found.");
            }
        } catch (SQLException e) {
            logger.warn("Exception in getContract", e);
            throw new ContractRetrievalException(e.getMessage());
        }
    }

    /**
     * Creates a new contractVersion with given details.
     *
     * @return True if a contract with given version was added successfully, False otherwise.
     * @throws DuplicateContractException If there already exists another contract with same contractId and versionName,
     *         then this method throws this exception.
     */
    public boolean addNewContractVersion(String contractId, String ownerAddress, String versionName, String address,
            String metaData, String byteCode, String sourceCode) throws DuplicateContractException {
        try {
            if (!hasContractVersion(contractId, versionName)) {
                insertNewVersionPstmt.setString(1, contractId);
                insertNewVersionPstmt.setString(2, versionName);
                insertNewVersionPstmt.setString(3, sourceCode);
                insertNewVersionPstmt.setString(4, byteCode);
                insertNewVersionPstmt.setString(5, metaData);
                insertNewVersionPstmt.setString(6, address);
                insertNewVersionPstmt.setString(7, ownerAddress);
                insertNewVersionPstmt.execute();
                return true;
            } else {
                throw new DuplicateContractException(
                        "ContractVersion with id " + contractId + " and version: " + versionName + " already exists.");
            }
        } catch (SQLException e) {
            logger.warn("Exception when adding contract: ", e);
        }
        return false;
    }

    /**
     * Updates an existing contractVersion with given details.
     *
     * @return True if a contract with given version was updated successfully, False otherwise.
     * @throws DuplicateContractException If there already exists another contract with same contractId and versionName,
     *         then this method throws this exception.
     */
    public boolean updateExistingContractVersion(String existingContractId, String existingVersionName,
            String contractId, String ownerAddress, String versionName, String metaData, String sourceCode)
            throws DuplicateContractException {
        try {
            if (!hasContractVersion(contractId, versionName)) {
                updateExistingVersionPstmt.setString(1, contractId);
                updateExistingVersionPstmt.setString(2, sourceCode);
                updateExistingVersionPstmt.setString(3, metaData);
                updateExistingVersionPstmt.setString(4, ownerAddress);
                updateExistingVersionPstmt.setString(5, versionName);
                updateExistingVersionPstmt.setString(6, existingContractId);
                updateExistingVersionPstmt.setString(7, existingVersionName);
                updateExistingVersionPstmt.execute();
                return true;
            } else {
                throw new DuplicateContractException(
                        "ContractVersion with id " + contractId + " and version: " + versionName + " already exists.");
            }
        } catch (SQLException e) {
            logger.warn("Exception when adding contract: ", e);
        }
        return false;
    }

    /**
     * Retrieve a BriefContractInfo about a particular contract.
     *
     * @param contractId Contract Id
     * @return The BriefInfo object
     * @throws ContractRetrievalException This exception is thrown when a contract with given contractId is not found.
     */
    public BriefContractInfo getBriefContractInfo(String contractId) throws ContractRetrievalException {
        try {
            getContractInfoPstmt.setString(1, contractId);
            ResultSet rs = getContractInfoPstmt.executeQuery();
            if (rs.isBeforeFirst()) {
                rs.next();
                BriefContractInfo bcInfo = new ContractVersion(rs.getString(CONTRACT_ID_COLUMN_LABEL),
                        rs.getString(CONTRACT_OWNER_COLUMN_LABEL));
                return bcInfo;
            } else {
                throw new ContractRetrievalException("Contract with contract ID: " + contractId + " not found.");
            }
        } catch (SQLException e) {
            logger.warn("Exception when getting contract list ", e);
            throw new ContractRetrievalException(e.getMessage());
        }
    }

    /**
     * Returns a list of BriefContractInfo objects, containing 1 BriefContractInfo object for every unique contractId
     * present in the database.
     *
     * @return The List of BriefContractInfo objects.
     */
    public List<BriefContractInfo> getAllBriefContractInfo() {
        // TODO: It is possible that contract list gets very big, then it
        // doesn't make sense to send everything over REST. Modify REST API
        // to handle that.
        List<BriefContractInfo> contracts = new ArrayList<>();
        try {
            ResultSet rs = getAllContractsShortPstmt.executeQuery();
            while (rs.next()) {
                contracts.add(new ContractVersion(rs.getString(CONTRACT_ID_COLUMN_LABEL),
                        rs.getString(CONTRACT_OWNER_COLUMN_LABEL)));
            }
        } catch (SQLException e) {
            logger.warn("Exception when getting contract list ", e);
        }
        return contracts;
    }

    /**
     * Retrieves BriefVersionInfo of all available versions of a contract identified by contractId.
     *
     * @param contractId Contract Id
     * @return Returns a list of all BriefVersionInfo objects.
     */
    public List<BriefVersionInfo> getAllBriefVersionInfo(String contractId) {
        List<BriefVersionInfo> versions = new ArrayList<>();
        try {
            getAllVersionsShortPstmt.setString(1, contractId);
            ResultSet rs = getAllVersionsShortPstmt.executeQuery();
            while (rs.next()) {
                ContractVersion cv = new ContractVersion();
                cv.setContractId(contractId);
                cv.setOwnerAddress(rs.getString(CONTRACT_OWNER_COLUMN_LABEL));
                cv.setAddress(rs.getString(CONTRACT_ADDRESS_COLUMN_LABEL));
                cv.setMetaData(rs.getString(CONTRACT_METADATA_COLUMN_LABEL));
                cv.setVersionName(rs.getString(CONTRACT_VERSION_COLUMN_LABEL));
                versions.add(cv);
            }
        } catch (SQLException e) {
            logger.warn("Exception when getting version list for contract: " + contractId, e);
        }
        return versions;
    }
}
