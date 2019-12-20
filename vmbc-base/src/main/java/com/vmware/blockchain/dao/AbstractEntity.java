/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.dao;

import java.util.Date;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonFilter;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.vmware.blockchain.common.DateTimeUtils;
import com.vmware.blockchain.db.DbEntityExclude;

import lombok.Data;

/**
 * Common entity definition, serve as base for any database entity.
 */
@Data
@JsonFilter(AbstractEntity.ENTITY_WRITER_FILTER_NAME)
public abstract class AbstractEntity {
    public static final String PROP_ID = "id";
    public static final String PROP_USER_ID = "userId";
    public static final String PROP_CREATED = "created";
    public static final String PROP_VERSION = "version";
    public static final String PROP_USERNAME = "userName";
    public static final String ENTITY_WRITER_FILTER_NAME = "HelenEntityWriterFilter";

    private UUID createUserId;
    private String createUserName;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = DateTimeUtils.ISO_8601_DATE_TIME_PATTERN)
    private Date created;

    private int version;
    private UUID id;

    /**
     * Below fields are derived from actual columns in the entity table.
     */
    @DbEntityExclude
    private UUID updatedByUserId;

    @DbEntityExclude
    private String updatedByUserName;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = DateTimeUtils.ISO_8601_DATE_TIME_PATTERN)
    @DbEntityExclude
    private Date updated;
}