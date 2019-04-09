/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Data structure for embedded Details.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class Details {
    private String firstName;
    private String lastName;
}
