/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.UUID;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Data structure for embedded Consortium.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ConsortiumData {
    private UUID consortiumId;
    private String consortiumName;
}

