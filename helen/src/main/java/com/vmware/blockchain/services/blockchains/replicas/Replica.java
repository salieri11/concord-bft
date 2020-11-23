/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.replicas;

import java.util.UUID;

import com.vmware.blockchain.dao.AbstractEntity;
import com.vmware.blockchain.dao.EntityColumnName;
import com.vmware.blockchain.dao.LinkedEntityId;
import com.vmware.blockchain.services.blockchains.NodeInterface;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * Entity representing a single replica.
 */
@EntityColumnName("helen.replica")
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
public class Replica extends AbstractEntity implements NodeInterface {
    String publicIp;
    String privateIp;
    String hostName;
    String url;
    String cert;

    @LinkedEntityId
    UUID zoneId;

    /**
     * Type of Replica.
     */
    @Deprecated
    public enum ReplicaType {
        // Only dealing with either NONE or DAML_PARTICIPANT for now
        // None: Ethereum, HFL, DAML Committer
        NONE,
        DAML_PARTICIPANT,
        READ_REPLICA
    }

    @Deprecated
    ReplicaType replicaType;

    @LinkedEntityId
    UUID blockchainId;
    String password;
}