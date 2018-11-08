/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 * A Spring Data JPA (or Hibernate) Entity class representing a consortium in the system.
 */
@Table(name = "CONSORTIUMS")
@Entity
public class Consortium {

    @OneToMany(mappedBy = "consortium", cascade = {CascadeType.PERSIST, CascadeType.REMOVE})
    protected Set<User> users = new HashSet<>();
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long consortiumId = 0L;
    private String consortiumName;
    private String consortiumType;

    protected Consortium() {}

    public Long getConsortiumId() {
        return consortiumId;
    }

    protected void setConsortiumId(Long consortiumId) {
        this.consortiumId = consortiumId;
    }

    public String getConsortiumName() {
        return consortiumName;
    }

    protected void setConsortiumName(String consortiumName) {
        this.consortiumName = consortiumName;
    }

    public String getConsortiumType() {
        return consortiumType;
    }

    protected void setConsortiumType(String consortiumType) {
        this.consortiumType = consortiumType;
    }

    public Set<User> getUsers() {
        return Collections.unmodifiableSet(users);
    }

    protected void addUser(User u) {
        users.add(u);
    }

    @Override
    public int hashCode() {
        return (int) ((consortiumId * 53) % 17);
    }

    @Override
    public boolean equals(Object o) {
        if (o == null || !(o instanceof Consortium)) {
            return false;
        }
        Consortium c = (Consortium) o;
        return c.getConsortiumId().equals(consortiumId);
    }
}
