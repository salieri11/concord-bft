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
 * A Spring Data JPA (or Hibernate) Entity class representing a consortium in the system.
 */
@Table(name = "CONSORTIUMS")
@Entity
@Data
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class Consortium {

    @OneToMany(mappedBy = "consortium", cascade = {CascadeType.PERSIST, CascadeType.REMOVE})
    protected Set<User> users = new HashSet<>();

    @OneToMany(mappedBy = "consortium", cascade = {CascadeType.PERSIST, CascadeType.REMOVE})
    protected Set<Blockchain> blockchains = new HashSet<>();

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "consortiumid")
    @EqualsAndHashCode.Include
    private UUID consortiumId;
    private String consortiumName;
    private String consortiumType;

    protected Consortium() {}

    protected void addUser(User u) {
        users.add(u);
    }
}
