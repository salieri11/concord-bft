/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.HashMap;
import java.util.Map;

import com.vmware.blockchain.dao.AbstractEntity;
import com.vmware.blockchain.dao.EntityColumnName;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * Entity representing Organization.
 */
@EntityColumnName("helen.organization")
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
public class Organization extends AbstractEntity {

    private String organizationName;

    private Map<String, String> organizationProperties = new HashMap<String, String>();

    public Organization(String organizationName) {
        this.organizationName = organizationName;
    }
}
