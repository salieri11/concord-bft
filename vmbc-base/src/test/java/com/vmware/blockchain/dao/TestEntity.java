/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.dao;

import java.time.Instant;
import java.time.ZonedDateTime;
import java.util.Date;
import java.util.UUID;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * Sample entity for testing.
 */
@EntityColumnName("test.entity")
@Data
@EqualsAndHashCode(callSuper = true, doNotUseGetters = true)
public class TestEntity extends AbstractEntity {

    @LinkedEntityId
    private UUID parentId1;

    @LinkedEntityId
    private UUID parentId2;

    @LinkedEntityId
    private UUID tenantId;

    private Date configured;

    private String body;

    private Integer age;

    private Double cost;

    private String others;

    private ZonedDateTime zoneDate;

    private Instant instant;
}
