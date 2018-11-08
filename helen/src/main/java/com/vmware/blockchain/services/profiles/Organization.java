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
 * A Spring Data JPA (or Hibernate) Entity class representing an organization in the system.
 */
@Table(name = "ORGANIZATIONS")
@Entity
public class Organization {
    @OneToMany(mappedBy = "organization", cascade = {CascadeType.PERSIST, CascadeType.REMOVE})
    protected Set<User> users = new HashSet<>();
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long organizationId = 0L;
    private String organizationName;

    protected Organization() {}

    public Long getOrganizationId() {
        return organizationId;
    }

    protected void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public String getOrganizationName() {
        return organizationName;
    }

    protected void setOrganizationName(String organizationName) {
        this.organizationName = organizationName;
    }

    public Set<User> getUsers() {
        return Collections.unmodifiableSet(users);
    }

    protected void addUser(User u) {
        users.add(u);
    }

    @Override
    public int hashCode() {
        return (int) ((organizationId * 53) % 17);
    }

    @Override
    public boolean equals(Object o) {
        if (o == null || !(o instanceof Organization)) {
            return false;
        }
        Organization org = (Organization) o;
        return org.getOrganizationId().equals(organizationId);
    }

}
