/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import java.util.UUID;

/**
 * Entity interface representing a node.
 * Should ideally be an abstract class.
 */
public interface NodeInterface {

    UUID getId();

    String getPublicIp();

    String getPrivateIp();

    UUID getZoneId();

    UUID getBlockchainId();

    String getUrl();
}