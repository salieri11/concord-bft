/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import static com.vmware.blockchain.services.blockchains.zones.Zone.LAT_KEY;
import static com.vmware.blockchain.services.blockchains.zones.Zone.LONG_KEY;
import static com.vmware.blockchain.services.blockchains.zones.Zone.NAME_KEY;

import java.util.HashMap;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.deployment.v1.BearerTokenCredential;
import com.vmware.blockchain.deployment.v1.Credential;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.IPv4Network;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.OutboundProxyInfo;
import com.vmware.blockchain.deployment.v1.PasswordCredential;
import com.vmware.blockchain.deployment.v1.TransportSecurity;
import com.vmware.blockchain.deployment.v1.VSphereDatacenterInfo;
import com.vmware.blockchain.deployment.v1.VSphereOrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.VmcOrchestrationSiteInfo;
import com.vmware.blockchain.services.blockchains.zones.OnpremZone;
import com.vmware.blockchain.services.blockchains.zones.VmcAwsZone;
import com.vmware.blockchain.services.blockchains.zones.Zone;
import com.vmware.blockchain.services.blockchains.zones.Zone.Type;

/**
 * Convienient utilities for Blockchain stuff.
 */
public class BlockchainUtils {
    private static Logger logger = LogManager.getLogger();

    private static ImmutableMap<Type, OrchestrationSiteInfo.Type> typeMap =
            ImmutableMap.of(Type.NONE, OrchestrationSiteInfo.Type.NONE,
                            Type.ON_PREM, OrchestrationSiteInfo.Type.VSPHERE,
                            Type.VMC_AWS, OrchestrationSiteInfo.Type.VMC);


    /**
     * Create a Fleet credential.
     */
    public static Credential toCredential(String user, String password) {
        return Credential.newBuilder()
                .setType(Credential.Type.PASSWORD)
                .setPasswordCredential(PasswordCredential.newBuilder().setUsername(user).setPassword(password).build())
                .build();

    }

    /**
     * Convert a Helen Zone to a Fleet Orchestration Site.
     */
    public static OrchestrationSiteInfo toInfo(Zone zone)  {

        if (Type.ON_PREM.equals(zone.getType())) {
            OnpremZone op = (OnpremZone) zone;
            if (op.getVCenter() == null) {
                logger.info("Missing required field vCenter");
                throw new BadRequestException(ErrorCode.BAD_REQUEST);
            }

            final Endpoint api = Endpoint.newBuilder()
                    .setAddress(op.getVCenter().getUrl())
                    .setCredential(toCredential(op.getVCenter().getUsername(), op.getVCenter().getPassword()))
                    .setTransportSecurity(TransportSecurity.newBuilder().build())
                    .build();

            Endpoint container = Endpoint.newBuilder().build();

            if (op.getContainerRepo() != null) {
                container = Endpoint.newBuilder()
                        .setAddress(op.getContainerRepo().getUrl())
                        .setCredential(toCredential(op.getContainerRepo().getUsername(),
                                                    op.getContainerRepo().getPassword()))
                        .setTransportSecurity(TransportSecurity.newBuilder().setType(TransportSecurity.Type.NONE))
                        .build();

            }
            Zone.Network n = op.getNetwork();
            if (n == null) {
                logger.info("Missing required field network");
                throw new BadRequestException(ErrorCode.BAD_REQUEST);
            }

            IPv4Network network = IPv4Network.newBuilder()
                    .setName(n.getName())
                    .setAddressAllocation(IPv4Network.AddressAllocationScheme.STATIC)
                    .setGateway(fromIpAddr(n.getGateway()))
                    .setSubnet(Integer.parseInt(n.getSubnet()))
                    .setAllocationServer(Endpoint.newBuilder().build())
                    .addAllNameServers(n.getNameServers())
                    .build();

            if (op.getResourcePool() == null || op.getResourcePool().isBlank()
                || op.getStorage() == null || op.getStorage().isBlank()
                || op.getFolder() == null || op.getFolder().isBlank()) {
                logger.info("Null or blank ResourcePool, Storage or Folder");
                throw new BadRequestException(ErrorCode.BAD_REQUEST);
            }

            OutboundProxyInfo outboundProxyInfo = OutboundProxyInfo.newBuilder().build();

            if (op.getOutboundProxy() != null) {
                outboundProxyInfo = OutboundProxyInfo.newBuilder()
                        .setHttpHost(op.getOutboundProxy().getHttpHost())
                        .setHttpPort(op.getOutboundProxy().getHttpPort())
                        .setHttpsHost(op.getOutboundProxy().getHttpsHost())
                        .setHttpsPort(op.getOutboundProxy().getHttpsPort())
                        .build();
            }

            VSphereDatacenterInfo dcInfo = VSphereDatacenterInfo.newBuilder()
                    .setResourcePool(op.getResourcePool())
                    .setDatastore(op.getStorage())
                    .setFolder(op.getFolder())
                    .setNetwork(network)
                    .setOutboundProxy(outboundProxyInfo)
                    .build();

            VSphereOrchestrationSiteInfo vspherInfo = VSphereOrchestrationSiteInfo.newBuilder()
                    .setApi(api)
                    .setContainerRegistry(container)
                    .setVsphere(dcInfo)
                    .build();

            return OrchestrationSiteInfo.newBuilder()
                    .setType(typeMap.get(zone.getType()))
                    .putAllLabels(toMap(zone))
                    .setVmc(VmcOrchestrationSiteInfo.newBuilder().build())
                    .setVsphere(vspherInfo)
                    .build();

        } else if (Type.VMC_AWS.equals(zone.getType())) {
            VmcAwsZone op = (VmcAwsZone) zone;
            Zone.Network n = op.getNetwork();

            IPv4Network network = IPv4Network.newBuilder()
                    .setName(n.getName())
                    .setAddressAllocation(IPv4Network.AddressAllocationScheme.STATIC)
                    .setGateway(fromIpAddr(n.getGateway()))
                    .setSubnet(Integer.parseInt(n.getSubnet()))
                    .setAllocationServer(Endpoint.newBuilder().build())
                    .build();

            VSphereDatacenterInfo dcInfo = VSphereDatacenterInfo.newBuilder()
                    .setResourcePool(op.getResourcePool())
                    .setDatastore(op.getStorage())
                    .setFolder(op.getFolder())
                    .setNetwork(network)
                    .build();

            Endpoint apiEndpoint = Endpoint.newBuilder()
                    .setAddress(op.getVmcUrl())
                    .build();

            Endpoint authenticationEndpoint = Endpoint.newBuilder()
                    .setAddress(op.getCspUrl())
                    .setCredential(Credential.newBuilder()
                                           .setType(Credential.Type.BEARER)
                                           .setTokenCredential(
                                                   BearerTokenCredential.newBuilder()
                                                           .setToken(op.getRefreshToken())
                                                           .build())
                                           .build())
                    .build();

            VmcOrchestrationSiteInfo vmcOrchestrationSiteInfo = VmcOrchestrationSiteInfo.newBuilder()
                    .setAuthentication(authenticationEndpoint)
                    .setApi(apiEndpoint)
                    .setOrganization(op.getOrganization())
                    .setDatacenter(op.getDatacenter())
                    .setVsphere(dcInfo)
                    .build();
            return OrchestrationSiteInfo.newBuilder()
                    .setType(typeMap.get(zone.getType()))
                    .putAllLabels(toMap(zone))
                    .setVmc(vmcOrchestrationSiteInfo)
                    .build();
        }
        throw new BadRequestException("Invalid zone type {}", zone.getType());
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
