/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import org.springframework.context.ApplicationEvent;

import lombok.Getter;

/**
 * BlockchainManagement Event.  published when a blockchain changes.
 */
@Getter
public class BlockchainManagerEvent extends ApplicationEvent {

    /**
     * What action triggered this event: Adding or deleting a node.
     */
    public enum Action {
        ADD_NODE,
        DELETE_NODE
    }

    private static final long serialVersionUID = 1L;
    private Blockchain blockchain;
    private Action action;
    private String node;

    /**
     * A new BlockchainManagerEvent.  This is triggered by adding/deleting a node to/from a blockchain.
     */
    public BlockchainManagerEvent(Object source, Blockchain blockchain, Action action, String node) {
        super(source);
        this.blockchain = blockchain;
        this.action = action;
        this.node = node;
    }

}
