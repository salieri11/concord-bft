/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.contracts;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * An interface for retrieving complete information about a particular contract.
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class FullVersionInfo extends BriefVersionInfo {
    String contractId;
    String owner;
    String bytecode;
    String sourcecode;

    /**
     * Create from contract.
     * @param c Contract
     */
    public FullVersionInfo(Contract c) {
        super(c);
        contractId = c.getName();
        owner = c.getOwner();
        bytecode = c.getBytecode();
        sourcecode = c.getSourcecode();
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

    /**
     * Check if address is the owners.
     * @param address Address in question
     * @return true if it's the owners
     */
    public boolean isOwner(String address) {
        if (!address.startsWith("0x")) {
            address = "0x" + address;
        }
        return owner.equals(address);
    }
}
