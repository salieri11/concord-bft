/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 */

package com.vmware.blockchain.services.profiles;

public class UserModificationException extends Exception {
   private static final long serialVersionUID = 1L;

   public UserModificationException(String message) {
      super(message);
   }
}
