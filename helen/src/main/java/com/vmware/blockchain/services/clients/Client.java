/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.clients;

import java.util.UUID;

import com.vmware.blockchain.dao.AbstractEntity;
import com.vmware.blockchain.dao.EntityColumnName;
import com.vmware.blockchain.dao.LinkedEntityId;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entity representing a single replica.
 */
@EntityColumnName("helen.client")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class Client extends AbstractEntity {
    String nodeId; //do we really need this?
    String clientGroupId; //up for discussion with ui
    @LinkedEntityId
    String blockchainId; //for metrics?
    UUID zoneId;

    /**
     * Indicate the type of replica.
     */
    public enum ClientType {
        // Only dealing with either NONE or DAML_PARTICIPANT for now
        // None: Ethereum, HFL, DAML Committer
        NONE,
        DAML_PARTICIPANT
    }

    ClientType clientType;
}
