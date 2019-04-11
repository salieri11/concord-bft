/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import java.util.List;
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
 * Class representing Blockchains.
 */
@EntityColumnName("helen.blockchain")
@Data
@EqualsAndHashCode(callSuper = true)
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Blockchain extends AbstractEntity {

    /**
     * A node entry.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class NodeEntry {
        UUID nodeId;
        String ip;
        String url;
        String cert;
        String region;
    }


    @LinkedEntityId
    UUID  consortium;

    List<NodeEntry> nodeList;
}
