/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Optional;
import java.util.UUID;

/**
 * An interface which defines the API for interacting with a user update (PATCH) request sent to the system.
 */
public interface UserPatchRequest {
    UUID getUserId();

    void setUserId(UUID userId);

    Optional<String> getOptionalRole();

    Optional<String> getOptionalFirstName();

    Optional<String> getOptionalLastName();

    Optional<String> getOptionalEmail();

    Optional<String> getOptionalName();

}
