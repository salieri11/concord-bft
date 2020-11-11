/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server.util;

import java.util.List;
import java.util.Map;

import com.vmware.blockchain.deployment.v1.IdentityComponent;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

/**
 * Holds all identity component list.
 */
@Builder
@Getter
@Setter
public class IdentityComponentsLists {
    private Map<String, List<IdentityComponent>> concordIdentityComponents;
    private Map<String, List<IdentityComponent>> bftIdentityComponents;
    private Map<String, List<IdentityComponent>> trsIdentityComponents;
    private Map<String, List<IdentityComponent>> trcIdentityComponents;
}
