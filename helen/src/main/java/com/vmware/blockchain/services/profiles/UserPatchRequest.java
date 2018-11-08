/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Optional;

/**
 * An interface which defines the API for interacting with a user update (PATCH) request sent to the system.
 */
public interface UserPatchRequest {
    Long getUserId();

    void setUserId(Long userId);

    Optional<String> getOptionalRole();

    Optional<String> getOptionalFirstName();

    Optional<String> getOptionalLastName();

    Optional<String> getOptionalEmail();

    Optional<String> getOptionalName();

}
