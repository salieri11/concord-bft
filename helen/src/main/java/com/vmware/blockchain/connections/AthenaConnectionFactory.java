/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.connections;

import java.io.IOException;
import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicLong;

import com.vmware.blockchain.common.AthenaProperties;

public final class AthenaConnectionFactory {
    private ConnectionType type;
    private AthenaProperties config;
    private ArrayList<Authority> athenaList;
    private AtomicLong nextAuthority;

    public enum ConnectionType {
        TCP, Mock
    }

    public AthenaConnectionFactory(ConnectionType type, AthenaProperties config) {
        this.type = type;
        this.config = config;
        nextAuthority = new AtomicLong();

        // Read list of athenas from config
        athenaList = new ArrayList<>();
        String authorities = config.getAthenaAuthorities();
        String[] authorityList = authorities.split(",");
        for (String authority : authorityList) {
            String[] group = authority.split(":");
            Authority a = new Authority(group[0], Integer.parseInt(group[1]));
            athenaList.add(a);
        }
    }

    public IAthenaConnection create() throws IOException, UnsupportedOperationException {
        switch (type) {
            case TCP:
                // Select an Athena instance to connect with in a round robin fashion
                int chosenAuthority = (int) nextAuthority.getAndIncrement() % athenaList.size();
                Authority athenaInstance = athenaList.get(chosenAuthority);
                AthenaTCPConnection connection =
                        new AthenaTCPConnection(config, athenaInstance.getHost(), athenaInstance.getPort());
                return connection;
            case Mock:
                return new MockConnection(config);
            default:
                throw new UnsupportedOperationException("type not supported" + type);
        }
    }

    private class Authority {
        String host;
        int port;

        Authority(String h, int p) {
            host = h;
            port = p;
        }

        String getHost() {
            return host;
        }

        int getPort() {
            return port;
        }
    }
}
