/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * A Spring Data JPA (or Hibernate) Entity class representing an organization in the system.
 */
@Table(name = "ORGANIZATIONS")
@Entity
@Data
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class Organization {
    @OneToMany(mappedBy = "organization", cascade = {CascadeType.PERSIST, CascadeType.REMOVE})
    protected Set<User> users = new HashSet<>();

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "organizationid")
    @EqualsAndHashCode.Include
    private UUID organizationId;
    private String organizationName;

    protected Organization() {}

    protected void addUser(User u) {
        users.add(u);
    }
}
