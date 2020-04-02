/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.model.vsphere;

import lombok.Builder;
import lombok.Data;

/**
 * Simplified binding of OVF property parameter.
 */
@Data
@Builder
public class OvfProperty {
    String id;
    String value;
}