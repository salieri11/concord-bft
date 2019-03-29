/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.UUID;

import com.vmware.blockchain.dao.AbstractEntity;
import com.vmware.blockchain.dao.EntityColumnName;
import com.vmware.blockchain.dao.LinkedEntityId;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * Class representing a consortium in the system.
 * Note that this class will link also link to organization.
 */
@EntityColumnName("helen.consortium")
@Data
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class Consortium extends AbstractEntity {

    private String consortiumName;
    private String consortiumType;

    // a consortium must belong to at least one organization
    @LinkedEntityId(biDirectional = true)
    private UUID organization;

}
