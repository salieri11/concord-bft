/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 */

package com.vmware.blockchain.services.profiles;

import java.util.Optional;

/**
 * An interface which defines the API for interacting with a user update
 * (PATCH) request sent to the system
 */
public interface UserPatchRequest {
   Long getUserID();

   void setUserID(Long userID);
   
   Optional<String> getOptionalRole();

   Optional<String> getOptionalFirstName();

   Optional<String> getOptionalLastName();

   Optional<String> getOptionalEmail();

   Optional<String> getOptionalName();
   
}
