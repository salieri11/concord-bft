/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;


import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 * A Spring Data JPA (or Hibernate) Entity class representing an keystore in
 * the system.
 */
@Table(name = "KEYSTORES")
@Entity
public class Keystore {

    @Id
    private String address;
    private String wallet;

    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    @JoinColumn(nullable = false)
    private User user;

    protected Keystore() {
    }

    public String getAddress() {
        return address;
    }

    protected void setAddress(String address) {
        this.address = address;
    }

    public String getWallet() {
        return wallet;
    }

    protected void setWallet(String wallet) {
        this.wallet = wallet;
    }

    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        Keystore keystore = (Keystore) o;

        if (!address.equals(keystore.address)) {
            return false;
        }
        return wallet.equals(keystore.wallet);
    }

    @Override
    public int hashCode() {
        int result = address.hashCode();
        result = 31 * result + wallet.hashCode();
        return result;
    }
}
