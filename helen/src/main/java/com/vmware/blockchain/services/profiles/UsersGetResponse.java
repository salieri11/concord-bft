/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.UUID;

import lombok.Builder;
import lombok.Data;

/**
 * A data object for response generated for a GET request in User management API.
 */
@Data
@Builder
public class UsersGetResponse {
    private UUID userId;
    private String name;
    private String email;
    private long lastLogin;
    private String role;
    private ConsortiumData consortium;
    private OrganizationData organization;
    private Details details;
}
