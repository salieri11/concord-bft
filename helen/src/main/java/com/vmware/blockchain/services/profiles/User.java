/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.time.Instant;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import com.vmware.blockchain.base.auth.Role;
import com.vmware.blockchain.dao.AbstractEntity;
import com.vmware.blockchain.dao.EntityColumnName;
import com.vmware.blockchain.dao.LinkedEntityId;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;


/**
 * Class representing a user in the system.
 * A user must belong to at least one organization
 */
@EntityColumnName("helen.user")
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@EqualsAndHashCode(callSuper = true)
public class User extends AbstractEntity {

    protected Set<Keystore> keystores = new HashSet<>();

    private String name;

    // firstName and lastName are primarily used for
    // internationalization purposes
    private String firstName;

    private String lastName;

    private String email;

    @Deprecated
    // Old role names.  This is for existing database entries.
    private List<Roles> roles;

    private List<Role> serviceRoles;

    @LinkedEntityId(biDirectional = true)
    private UUID organization;

    private String password;

    private Instant lastLogin;

}