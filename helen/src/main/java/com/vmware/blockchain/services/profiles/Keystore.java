/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.UUID;

import com.vmware.blockchain.dao.AbstractEntity;
import com.vmware.blockchain.dao.EntityColumnName;
import com.vmware.blockchain.dao.LinkedEntityId;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * Entity class representing an keystore in
 * the system.
 */
@EntityColumnName("helen.keystore")
@Data
@EqualsAndHashCode(callSuper = true)
public class Keystore extends AbstractEntity {

    private String address;
    private String wallet;

    @LinkedEntityId
    private UUID user;
}
