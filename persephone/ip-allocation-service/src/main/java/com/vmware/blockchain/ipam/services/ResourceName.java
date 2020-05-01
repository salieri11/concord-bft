/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.ipam.services;

import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * Data class for resource name.
 */
@AllArgsConstructor
@Data
public class ResourceName {

    private String value = "";
}