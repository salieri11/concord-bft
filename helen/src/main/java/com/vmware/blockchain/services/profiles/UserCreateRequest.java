/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Optional;

/**
 * An interface which defines the API for interacting with a new user creation request in user management system
 */
public interface UserCreateRequest {
    String getUserName();

    Optional<String> getOptionalFirstName();

    Optional<String> getOptionalLastName();

    String getEmail();

    String getRole();

    String getPassword();

    Long getOrganizationID();

    Long getConsortiumID();
}
