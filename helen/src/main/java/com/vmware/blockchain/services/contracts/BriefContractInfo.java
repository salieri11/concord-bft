package com.vmware.blockchain.services.contracts;

/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 * 
 * An interface for retrieving brief information about a particular contract
 */
public interface BriefContractInfo {
   String getContractId();

   String getOwnerAddress();
}
