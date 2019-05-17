/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.contracts;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Data structure for BriefContractInfo.
 */
@Data
@NoArgsConstructor
public class BriefContractInfo {
    String contractId;
    String owner;

    // For brief contract info, include the url, ignore otherwise
    @JsonInclude(Include.NON_NULL)
    String url;

    // for get contract id, include list of versions
    @JsonInclude(Include.NON_NULL)
    List<BriefVersionInfo> versions;

    /**
     * New version created from contract.
     * @param c Contract
     */
    public BriefContractInfo(Contract c) {
        contractId = c.getName();
        owner = c.getOwner();
    }
}
