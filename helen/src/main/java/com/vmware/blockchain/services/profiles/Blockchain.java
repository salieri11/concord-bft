/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.ToString;

/**
 * JPA class representing Blockchains.
 */
@Table(name = "BLOCKCHAINS")
@Entity
@Data
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class Blockchain {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @EqualsAndHashCode.Include
    private UUID id;

    @OneToOne(optional = false, fetch = FetchType.LAZY)
    @JoinColumn(nullable = false)
    private Consortium consortium;

    /**
     * Comma separated list of IP addresses for each node in the chain.  On insert,
     * we normalize this list.  No two blockchains can share the same list of nodes.
     */
    @Column(nullable = false, unique = true)
    private String ipList;

    /**
     * Return the ipList string as a list of strings.
     * @return List of IP addresses.
     */
    public List<String> getIpAsList() {
        // removes leading and trailing whitespace, and any whitespace in between
        String[] a = getIpList().trim().split("\\s*,\\s*");
        return Arrays.asList(a);
    }

    /**
     * Save the list of IPs into the ipList string, a csv.
     * @param ips String list ist of ip addresses of nodes
     */
    public void setIpAsList(List<String> ips) {
        setIpList(String.join(",", ips));
    }


}
