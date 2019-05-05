/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.contracts;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

/**
 * Represents one specific version of a specific contract. This class also represents a single row in the `Contracts`
 * table of the database.
 */
@Getter
@Setter
@AllArgsConstructor
@Builder
public class ContractVersion {
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

    /**
     * Check if address is the owners.
     * @param address Address in question
     * @return true if it's the owners
     */
    public boolean isOwner(String address) {
        if (!address.startsWith("0x")) {
            address = "0x" + address;
        }
        return ownerAddress.equals(address);
    }
}
