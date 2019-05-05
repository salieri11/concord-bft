/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.contracts;

import java.util.UUID;

import com.vmware.blockchain.dao.AbstractEntity;
import com.vmware.blockchain.dao.EntityColumnName;
import com.vmware.blockchain.dao.LinkedEntityId;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * Contract Entity.
 */
@EntityColumnName("helen.contract")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@EqualsAndHashCode(callSuper = true)
public class Contract extends AbstractEntity {
    private String name;

    private String versionName;

    private String address;

    private String sourcecode;

    private String bytecode;

    private String metadata;

    private String owner;

    @LinkedEntityId
    private UUID blockchainId;

}
