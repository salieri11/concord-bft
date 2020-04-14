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
    String logicalClientId;
    String nodeId;
    @LinkedEntityId
    String blockchainId;
    UUID zoneId;
    /**
     * Indicate the type of replica.
     */

    public enum ClientType {
        NONE,
        DAML_PARTICIPANT
    }

    ClientType clientType;
}
