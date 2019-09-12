/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Date;
import java.util.UUID;

import com.vmware.blockchain.dao.AbstractEntity;
import com.vmware.blockchain.dao.EntityColumnName;
import com.vmware.blockchain.dao.LinkedEntityId;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * Agreement entity.
 */
@EntityColumnName("helen.agreement")
@Data
@EqualsAndHashCode(callSuper = true)
public class Agreement extends AbstractEntity {

    private boolean accepted;

    private String type;

    private String content;

    private String firstName;

    private String lastName;

    private String company;

    private Date acceptedOn;

    @LinkedEntityId
    private UUID orgId;
}
