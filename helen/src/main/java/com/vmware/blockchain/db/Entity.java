/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.db;

import java.util.Date;
import java.util.UUID;

import lombok.Getter;
import lombok.Setter;

/**
 * This represents a row in either the entity or entity_history table.
 */
@Getter
@Setter
public class Entity {
    private long createdId;
    private UUID rowKey;
    private String columnName;
    private int version;
    private String body;
    private UUID userId;
    private String userName;
    private Date createdTms;

    // Jackson needs this contructor
    public Entity() {}

    // For some reason, mybatis uses these constructors
    public Entity(UUID rowKey) {
        this.rowKey = rowKey;
    }

    public Entity(UUID rowKey, UUID userId) {
        this.rowKey = rowKey;
        this.userId = userId;
    }

}
