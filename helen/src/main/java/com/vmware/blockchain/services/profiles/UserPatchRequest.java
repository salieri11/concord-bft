/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.UUID;

import lombok.Data;

/**
 * An interface which defines the API for interacting with a user update (PATCH) request sent to the system.
 */
@Data
public class UserPatchRequest {
    UUID userId;
    String role;
    String email;
    String name;
    Details details;
}
