/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.clients;

import java.util.UUID;

import com.vmware.blockchain.dao.AbstractEntity;
import com.vmware.blockchain.dao.EntityColumnName;
import com.vmware.blockchain.dao.LinkedEntityId;
import com.vmware.blockchain.services.blockchains.NodeInterface;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entity representing a single client.
 */
@EntityColumnName("helen.client")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class Client extends AbstractEntity implements NodeInterface {

    String publicIp;
    String privateIp;
    String password;
    String url;
    String authJwtUrl;

    @LinkedEntityId
    UUID blockchainId;

    @LinkedEntityId
    UUID zoneId;
    // Logical Group Index Id supplied by the caller.
    //String groupIndex;
    // Id for the logical group that this client belongs to.
    UUID groupId;
}
