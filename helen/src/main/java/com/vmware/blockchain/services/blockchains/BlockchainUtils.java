/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import static com.vmware.blockchain.services.blockchains.zones.Zone.LAT_KEY;
import static com.vmware.blockchain.services.blockchains.zones.Zone.LONG_KEY;
import static com.vmware.blockchain.services.blockchains.zones.Zone.NAME_KEY;

import java.util.HashMap;
import java.util.Map;

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.deployment.v1.BearerTokenCredential;
import com.vmware.blockchain.deployment.v1.Credential;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.IPv4Network;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.PasswordCredential;
import com.vmware.blockchain.deployment.v1.TransportSecurity;
import com.vmware.blockchain.deployment.v1.VSphereDatacenterInfo;
import com.vmware.blockchain.deployment.v1.VSphereOrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.VmcOrchestrationSiteInfo;
import com.vmware.blockchain.services.blockchains.zones.OnpremZone;
import com.vmware.blockchain.services.blockchains.zones.Zone;
import com.vmware.blockchain.services.blockchains.zones.Zone.Type;

/**
 * Convienient utilities for Blockchain stuff.
 */
public class BlockchainUtils {
    private static ImmutableMap<Type, OrchestrationSiteInfo.Type> typeMap =
            ImmutableMap.of(Type.NONE, OrchestrationSiteInfo.Type.NONE,
                            Type.ON_PREM, OrchestrationSiteInfo.Type.VSPHERE,
                            Type.VMC_AWS, OrchestrationSiteInfo.Type.VMC);


    /**
     * Create a Fleet credential.
     */
    public static Credential toCredential(String user, String password) {
        return new Credential(Credential.Type.PASSWORD,
                              new PasswordCredential(user, password),
                              new BearerTokenCredential());

    }

    /**
     * Convert a Helen Zone to a Fleet Orchestration Site.
     */
    public static OrchestrationSiteInfo toInfo(Zone zone)  {
        VSphereOrchestrationSiteInfo vspherInfo = new VSphereOrchestrationSiteInfo();
        if (Type.ON_PREM.equals(zone.getType())) {
            OnpremZone op = (OnpremZone) zone;
            Endpoint api = new Endpoint(op.getVCenter().getUrl(),
                                        toCredential(op.getVCenter().getUsername(), op.getVCenter().getPassword()),
                                        new TransportSecurity());
            Endpoint container = new Endpoint(op.getContainerRepo().getUrl(),
                                              toCredential(op.getContainerRepo().getUsername(),
                                                           op.getContainerRepo().getPassword()),
                                              new TransportSecurity());
            Zone.Network n = op.getNetwork();
            IPv4Network network = new IPv4Network(n.getName(),
                                                  IPv4Network.AddressAllocationScheme.STATIC,
                                                  fromIpAddr(n.getGateway()),
                                                  Integer.parseInt(n.getSubnet()),
                                                  new Endpoint(),
                                                  n.getNameServers());
            VSphereDatacenterInfo dcInfo =
                    new VSphereDatacenterInfo(op.getResourcePool(), op.getStorage(), op.getFolder(), network);
            vspherInfo = new VSphereOrchestrationSiteInfo(api, container, dcInfo);
        }

        return new OrchestrationSiteInfo(typeMap.get(zone.getType()),
                                         toMap(zone),
                                         new VmcOrchestrationSiteInfo(),
                                         vspherInfo);
    }

    private static int fromIpAddr(String ipAddr) {
        String[] ips = ipAddr.split("\\.");
        int result = 0;
        for (int i = 0; i < 4; i++) {
            int ip = Integer.parseInt(ips[i]);
            result <<= 8;
            result |= ip;
        }
        return result;
    }

    private static Map<String, String> toMap(Zone z) {
        Map<String, String> map = new HashMap<>();
        if (z.getName() != null) {
            map.put(NAME_KEY, z.getName());
        }
        if (z.getLatitude() != null) {
            map.put(LAT_KEY, z.getLatitude());
        }
        if (z.getLongitude() != null) {
            map.put(LONG_KEY, z.getLongitude());
        }
        return map;
    }
}
