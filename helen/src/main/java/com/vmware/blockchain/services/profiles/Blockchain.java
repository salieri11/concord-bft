/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.google.common.base.Splitter;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * JPA class representing Blockchains.
 */
@Table(name = "BLOCKCHAINS")
@Entity
@Data
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
@AllArgsConstructor
@NoArgsConstructor
public class Blockchain {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @EqualsAndHashCode.Include
    private UUID id;

    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    @JoinColumn(nullable = false)
    private Consortium consortium;

    /**
     * IP List. host:port
     */
    private String ipList;

    /**
     * rpcUrls.  a map host=URL.
     */
    private String rpcUrls;

    /**
     * rpcCerts.  a map host=Cert.
     */
    private String rpcCerts;

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

    /**
     * Return the rpcUrls string as a map of host strings to url strings.
     * @return Map of URLs
     */
    public Map<String, String> getUrlsAsMap() {
        // removes leading and trailing whitespace, and any whitespace in between
        return Splitter.on(",").withKeyValueSeparator("=").split(getRpcUrls());
    }

    /**
     * Save the list of RPC URLSs into the rpcUrls string, a csv.
     * @param urls Map of RPC URLs of nodes
     */
    public void setUrlsAsMap(Map<String, String> urls) {
        List<String> hosts = urls.entrySet().stream().map(e -> String.format("%s=%s", e.getKey(), e.getValue()))
                .collect(Collectors.toList());
        setRpcUrls(String.join(",", hosts));
    }

    /**
     * Return the rpcCerts string as a map of host strings to cert strings.
     * @return Map of certificates
     */
    public Map<String, String> getCertsAsMap() {
        // removes leading and trailing whitespace, and any whitespace in between
        return Splitter.on(",").withKeyValueSeparator("=").split(getRpcCerts());
    }

    /**
     * Save the list of certs into the rpcCerts string, a csv.
     * @param certs Map of certificates of nodes
     */
    public void setCertsAsMap(Map<String, String> certs) {
        List<String> hosts = certs.entrySet().stream().map(e -> String.format("%s=%s", e.getKey(), e.getValue()))
                .collect(Collectors.toList());
        setRpcCerts(String.join(",", hosts));
    }

}
