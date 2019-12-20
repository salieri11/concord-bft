/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.dao;

import java.util.Date;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.vmware.blockchain.common.DateTimeUtils;

import lombok.Value;

/**
 * Entity summary - just include most basic columns in DB - nothing from body.
 */
@Value
public class EntitySummary {
    private UUID id;
    private int version;
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = DateTimeUtils.ISO_8601_SIMPLE_DATE_PATTERN)
    private Date updated;
}
