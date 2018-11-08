/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.contracts;

/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 * Represents one specific version of a specific contract. This class also represents a single row in the `Contracts`
 * table of the database.
 */
public class ContractVersion implements FullVersionInfo {
    // Id of this contract
    private String contractId;
    // address of the owner of this contract
    private String ownerAddress;
    // List of different version names of this contracts in creation order
    private String versionName;
    // Version address
    private String address;
    // Metadata of this version
    private String metaData;
    // Bytecode of this version
    private String byteCode;
    // source code of this version
    private String sourceCode;

    protected ContractVersion() {}

    protected ContractVersion(String id, String ownerAddress) {
        this.contractId = id;
        this.ownerAddress = ownerAddress;
    }

    protected void setContractId(String contractId) {
        this.contractId = contractId;
    }

    protected void setVersionName(String versionName) {
        this.versionName = versionName;
    }

    protected void setAddress(String address) {
        this.address = address;
    }

    protected void setMetaData(String metaData) {
        this.metaData = metaData;
    }

    protected void setByteCode(String byteCode) {
        this.byteCode = byteCode;
    }

    protected void setSourceCode(String sourceCode) {
        this.sourceCode = sourceCode;
    }

    protected void setOwnerAddress(String ownerAddress) {

        this.ownerAddress = ownerAddress;
    }

    protected ContractVersion(String id, String ownerAddress, String versionName, String versionAddress,
            String versionMetadata, String versionBytecode, String versionSourcecode) {
        this.contractId = id;

        this.ownerAddress = ownerAddress;
        this.versionName = versionName;
        this.address = versionAddress;
        this.metaData = versionMetadata;
        this.byteCode = versionBytecode;
        this.sourceCode = versionSourcecode;
    }

    /**
     * Checks if address is a syntactically valid contract address or not. This method does NOT actually check if
     * contract is deployed at that address or not. It simply validates that address is exactly 40 chars long (20 bytes
     * - excluding `0x`) and matches the regex for hex numbers
     *
     * @param hash The address which needs to be validated
     * @return Returns true if address is a valid contract address, false otherwise
     */
    public static boolean isValidContractAddress(String hash) {
        // has to be 20 byte hex string = 40 char string
        // we allows it to either start with 0x or not
        if (hash.startsWith("0x")) {
            hash = hash.substring(2);
        }
        return hash.length() == 40 && hash.matches("[0-9a-zA-Z]+");
    }

    public String getContractId() {
        return contractId;
    }

    public String getOwnerAddress() {
        return ownerAddress;
    }

    public String getVersionName() {
        return versionName;
    }

    public boolean isOwner(String address) {
        if (!address.startsWith("0x")) {
            address = "0x" + address;
        }
        return ownerAddress.equals(address);
    }

    public String getByteCode() {
        return byteCode;
    }

    public String getMetaData() {
        return metaData;
    }

    public String getSourceCode() {
        return sourceCode;
    }

    public String getAddress() {
        return address;
    }

}
