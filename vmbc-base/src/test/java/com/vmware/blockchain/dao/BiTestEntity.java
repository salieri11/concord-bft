/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.dao;

import java.util.UUID;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * Entity for testing bidirection linking.
 */
@EntityColumnName("test.bitestentity")
@Data
@EqualsAndHashCode(callSuper = true, doNotUseGetters = true)
public class BiTestEntity extends AbstractEntity {

    @LinkedEntityId(biDirectional = true)
    UUID testEntity;

}
