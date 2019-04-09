/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import lombok.Data;

/**
 * Data object for interacting with a new user creation request in user management system.
 */
@Data
public class UserCreateRequest {
    String name;
    String email;
    String role;
    String password;
    OrganizationData organization;
    ConsortiumData consortium;
    Details details;
}
