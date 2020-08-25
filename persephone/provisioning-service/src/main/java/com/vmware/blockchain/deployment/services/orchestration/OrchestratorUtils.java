/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration;

import java.security.KeyStore;
import java.util.AbstractMap;

import javax.net.ssl.SSLContext;

import org.apache.http.client.HttpClient;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.ssl.SSLContextBuilder;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;

import com.google.common.net.InetAddresses;
import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.services.orchestration.ipam.IpamClient;
import com.vmware.blockchain.deployment.v1.Address;

/**
 * common utilities for orchestrators.
 */
public class OrchestratorUtils {

    /**
     * get ip address to allocate.
     * @param ipamClient ipamClient
     * @param networkSegmentName networkSegmentName
     * @param ip ip in string
     * @param name name
     * @return pair of name and ip address as string.
     */
    public static AbstractMap.SimpleEntry<String, String> getAddress(IpamClient ipamClient, String networkSegmentName,
                                                                     String ip, String name) {
        if (ip.isEmpty()) {
            Address add = ipamClient.allocatedPrivateIp(networkSegmentName);
            return new AbstractMap.SimpleEntry<>(add.getName(),
                    InetAddresses.fromInteger(add.getValue()).getHostAddress());
        }

        if (!InetAddresses.isInetAddress(ip)) {
            throw new IllegalArgumentException("Provided ip is not a valid Inet address. ip: " + ip);
        }

        return new AbstractMap.SimpleEntry<>(name, ip);
    }

    public static HttpComponentsClientHttpRequestFactory getHttpRequestFactoryGivenKeyStore(KeyStore keyStore) {
        try {
            SSLContext sslContext = new SSLContextBuilder()
                    .loadTrustMaterial(keyStore, null)
                    .build();

            SSLConnectionSocketFactory socketFactory = new SSLConnectionSocketFactory(sslContext);

            HttpClient httpClient = HttpClients.custom()
                    .setSSLSocketFactory(socketFactory)
                    .build();

            HttpComponentsClientHttpRequestFactory factory =
                    new HttpComponentsClientHttpRequestFactory(httpClient);
            return factory;
        } catch (Exception e) {
            throw new PersephoneException(e, "Error Creating Keystore");
        }
    }
}
