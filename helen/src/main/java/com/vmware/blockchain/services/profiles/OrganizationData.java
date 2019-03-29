/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.UUID;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Data structure for embedded Organization.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class OrganizationData {
    private UUID organizationId;
    private String organizationName;
}

