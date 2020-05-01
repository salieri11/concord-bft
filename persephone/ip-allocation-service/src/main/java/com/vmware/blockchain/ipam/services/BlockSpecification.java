/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.ipam.services;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Class representing BlockSpecification.
 */
@AllArgsConstructor
@Data
@NoArgsConstructor
public class BlockSpecification {

    int prefix;
    int subnet;
}