package com.vmware.blockchain.services.contracts;

/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 */
public class DuplicateContractException extends Exception {
   private static final long serialVersionUID = 1L;

   public DuplicateContractException(String message) {
      super(message);
   }
}
