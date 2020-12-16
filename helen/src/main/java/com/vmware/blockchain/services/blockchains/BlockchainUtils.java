/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import static com.vmware.blockchain.services.blockchains.zones.Zone.LAT_KEY;
import static com.vmware.blockchain.services.blockchains.zones.Zone.LONG_KEY;
import static com.vmware.blockchain.services.blockchains.zones.Zone.NAME_KEY;

import java.io.ByteArrayInputStream;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import com.google.common.base.Strings;
import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.ErrorCodeType;
import com.vmware.blockchain.deployment.v1.BearerTokenCredential;
import com.vmware.blockchain.deployment.v1.Credential;
import com.vmware.blockchain.deployment.v1.ElasticSearch;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.IPv4Network;
import com.vmware.blockchain.deployment.v1.LogManagement;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.OutboundProxyInfo;
import com.vmware.blockchain.deployment.v1.PasswordCredential;
import com.vmware.blockchain.deployment.v1.TransportSecurity;
import com.vmware.blockchain.deployment.v1.VSphereDatacenterInfo;
import com.vmware.blockchain.deployment.v1.VSphereOrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.VmcOrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.Wavefront;
import com.vmware.blockchain.services.blockchains.zones.OnPremZone;
import com.vmware.blockchain.services.blockchains.zones.VmcAwsZone;
import com.vmware.blockchain.services.blockchains.zones.Zone;
import com.vmware.blockchain.services.blockchains.zones.Zone.Type;
import com.vmware.blockchain.services.profiles.Organization;

/**
 * Convenient utilities for Blockchain stuff.
 */
@Component
public class BlockchainUtils {
    private static Logger logger = LogManager.getLogger();
    private static String lintEndpoint;
    private static String lintAuthBearer;

    private static String wavefrontEndpoint;
    private static String wavefrontToken;

    @Autowired
    public BlockchainUtils(@Value("${lint.logging.endpoint}") String lintEndpoint,
                           @Value("${lint.logging.authbearer}") String lintAuthBearer,
                           @Value("${default.wavefront.endpoint}") String wavefrontEndpoint,
                           @Value("${default.wavefront.token}") String wavefrontToken) {
        BlockchainUtils.lintEndpoint = lintEndpoint;
        BlockchainUtils.lintAuthBearer = lintAuthBearer;
        BlockchainUtils.wavefrontEndpoint = wavefrontEndpoint;
        BlockchainUtils.wavefrontToken = wavefrontToken;
    }

    private static ImmutableMap<Type, OrchestrationSiteInfo.Type> typeMap =
            ImmutableMap.of(Type.NONE, OrchestrationSiteInfo.Type.NONE,
                            Type.ON_PREM, OrchestrationSiteInfo.Type.VSPHERE,
                            Type.VMC_AWS, OrchestrationSiteInfo.Type.VMC);

    /**
     * Create a Fleet Password Credential.
     */
    private static Credential toPasswordCredential(String user, String password) {
        return Credential.newBuilder()
                .setType(Credential.Type.PASSWORD)
                .setPasswordCredential(PasswordCredential.newBuilder()
                        .setUsername(user)
                        .setPassword(password)
                        .build())
                .build();
    }

    /**
     * Create a Fleet Bearer Credential.
     */
    private static Credential toBearerCredential(String bearer) {
        return Credential.newBuilder()
                .setType(Credential.Type.BEARER)
                .setTokenCredential(BearerTokenCredential.newBuilder()
                        .setToken(bearer)
                        .build())
                .build();
    }

    /**
     * Convert a Helen Zone to a Fleet Orchestration Site.
     */
    public static OrchestrationSiteInfo toInfo(Zone zone, Organization organization)  {
        Zone.Wavefront wf = zone.getWavefront();
        Wavefront wavefront = Wavefront.newBuilder().build();

        Zone.Elasticsearch es = zone.getElasticsearch();
        ElasticSearch elasticSearch = ElasticSearch.newBuilder().build();

        if (wf != null && !Strings.isNullOrEmpty(wf.getUrl())) {
            wavefront = Wavefront.newBuilder()
                    .setUrl(wf.getUrl())
                    .setToken(wf.getToken())
                    .build();
        } else if (organization.getOrganizationProperties().getOrDefault(Constants.USE_WAVEFRONT, "True")
                .equalsIgnoreCase("False")) {
            logger.info("Use of wavefront is set to false. Will not send metrics to wavefront.");
        } else {
            // Hack to default to our SRE wavefront.
            wavefront = Wavefront.newBuilder()
                    .setUrl(wavefrontEndpoint)
                    .setToken(wavefrontToken)
                    .build();
        }

        if (es != null) {
            elasticSearch = ElasticSearch.newBuilder()
                    .setUrl(es.getUrl())
                    .setUsername(es.getUsername())
                    .setPassword(es.getPassword())
                    .build();
        }

        if (Type.ON_PREM.equals(zone.getType())) {
            OnPremZone op = (OnPremZone) zone;
            if (op.getVCenter() == null) {
                logger.info("Missing required field vcenter");
                throw new BadRequestException(ErrorCodeType.BAD_REQUEST);
            }

            final Endpoint api = Endpoint.newBuilder()
                    .setAddress(op.getVCenter().getUrl())
                    .setCredential(toPasswordCredential(op.getVCenter().getUsername(), op.getVCenter().getPassword()))
                    .setTransportSecurity(TransportSecurity.newBuilder().build())
                    .build();

            Endpoint.Builder containerBuilder = Endpoint.newBuilder();

            if (op.getContainerRepo() != null) {
                containerBuilder = Endpoint.newBuilder()
                        .setAddress(op.getContainerRepo().getUrl())
                        .setCredential(toPasswordCredential(op.getContainerRepo().getUsername(),
                                                    op.getContainerRepo().getPassword()));

                if (StringUtils.hasText(op.getContainerRepo().getTlsCertificateData())) {
                    try {
                        // Implicitly checks for the Certificate Parsing and Encoding Exceptions
                        X509Certificate tlsCertificate = (X509Certificate) CertificateFactory.getInstance("X.509")
                                .generateCertificate(new ByteArrayInputStream(op.getContainerRepo()
                                                                                      .getTlsCertificateData()
                                                                                      .getBytes()));
                        // Throws exception if certificate has expired or not yet valid
                        tlsCertificate.checkValidity();
                    } catch (Exception e) {
                        logger.error("Container Repo's certificate data is invalid, error: ", e);
                        throw new BadRequestException(ErrorCodeType.CONTAINER_BAD_CERTIFICATE);
                    }
                    containerBuilder.setTransportSecurity(
                            TransportSecurity.newBuilder().setType(TransportSecurity.Type.TLSv1_2)
                                    .setCertificateData(op.getContainerRepo().getTlsCertificateData()).build());
                } else {
                    containerBuilder.setTransportSecurity(
                            TransportSecurity.newBuilder().setType(TransportSecurity.Type.TLSv1_2).build());
                }
            }

            Endpoint.Builder notaryServerBuilder = Endpoint.newBuilder();
            if (op.getNotaryServer() != null) {
                if (op.getContainerRepo() == null) {
                    logger.error("Notary Server provided but container repo not provided");
                    throw new BadRequestException(ErrorCodeType.NOTARY_PROVIDED_BUT_CONTAINER_REPO_EMPTY);
                }
                if (StringUtils.hasText(op.getNotaryServer().getUrl())) {
                    notaryServerBuilder.setAddress(op.getNotaryServer().getUrl());
                    if (StringUtils.hasText(op.getNotaryServer().getTlsCertificateData())) {
                        try {
                            // Implicitly checks for the Certificate Parsing and Encoding Exceptions
                            X509Certificate tlsCertificate = (X509Certificate) CertificateFactory.getInstance("X.509")
                                    .generateCertificate(new ByteArrayInputStream(op.getNotaryServer()
                                                                                          .getTlsCertificateData()
                                                                                          .getBytes()));
                            // Throws exception if certificate has expired or not yet valid
                            tlsCertificate.checkValidity();
                        } catch (Exception e) {
                            logger.error("Notary Server's certificate data is invalid, error: ", e);
                            throw new BadRequestException(ErrorCodeType.NOTARY_BAD_CERTIFICATE);
                        }
                        notaryServerBuilder.setTransportSecurity(
                                TransportSecurity.newBuilder().setType(TransportSecurity.Type.TLSv1_2)
                                        .setCertificateData(op.getNotaryServer().getTlsCertificateData()).build());
                    }
                } else if (StringUtils.hasText(op.getNotaryServer().getTlsCertificateData())) {
                    logger.error("Notary Server's certificate data provided but url not provided");
                    throw new BadRequestException(ErrorCodeType.NOTARY_URL_EMPTY_BUT_CERT_PROVIDED);
                }
            }

            Zone.Network n = op.getNetwork();
            if (n == null) {
                logger.info("Missing required field network");
                throw new BadRequestException(ErrorCodeType.BAD_REQUEST);
            }

            IPv4Network network = IPv4Network.newBuilder()
                    .setName(n.getName())
                    .setAddressAllocation(IPv4Network.AddressAllocationScheme.STATIC)
                    .setGatewayIp(n.getGateway())
                    .setSubnet(Integer.parseInt(n.getSubnet()))
                    .setAllocationServer(Endpoint.newBuilder().build())
                    .addAllNameServers(n.getNameServers() == null ? Arrays.asList() : n.getNameServers())
                    .build();

            if (op.getResourcePool() == null || op.getResourcePool().isBlank()
                || op.getStorage() == null || op.getStorage().isBlank()
                || op.getFolder() == null || op.getFolder().isBlank()) {
                logger.info("Null or blank ResourcePool, Storage or Folder");
                throw new BadRequestException(ErrorCodeType.BAD_REQUEST);
            }

            OutboundProxyInfo outboundProxyInfo = OutboundProxyInfo.newBuilder().build();

            if (op.getOutboundProxy() != null) {
                // create a builder, and set non-null fields
                OutboundProxyInfo.Builder b = OutboundProxyInfo.newBuilder();
                if (op.getOutboundProxy().getHttpHost() != null) {
                    b.setHttpHost(op.getOutboundProxy().getHttpHost());
                }
                if (op.getOutboundProxy().getHttpsHost() != null) {
                    b.setHttpsHost(op.getOutboundProxy().getHttpsHost());
                }
                b.setHttpPort(op.getOutboundProxy().getHttpPort());
                b.setHttpsPort(op.getOutboundProxy().getHttpsPort());
                outboundProxyInfo =  b.build();
            }

            VSphereDatacenterInfo dcInfo = VSphereDatacenterInfo.newBuilder()
                    .setResourcePool(op.getResourcePool())
                    .setDatastore(op.getStorage())
                    .setFolder(op.getFolder())
                    .setNetwork(network)
                    .setOutboundProxy(outboundProxyInfo)
                    .build();

            VSphereOrchestrationSiteInfo vSphereInfo = VSphereOrchestrationSiteInfo.newBuilder()
                    .setApi(api)
                    .setContainerRegistry(containerBuilder.build())
                    .setNotaryServer(notaryServerBuilder.build())
                    .setVsphere(dcInfo)
                    .setWavefront(wavefront)
                    .setElasticsearch(elasticSearch)
                    .addAllLogManagements(toFleetLogManagements(op))
                    .build();

            return OrchestrationSiteInfo.newBuilder()
                    .setType(typeMap.get(zone.getType()))
                    .putAllLabels(toMap(zone))
                    .setVmc(VmcOrchestrationSiteInfo.newBuilder().build())
                    .setVsphere(vSphereInfo)
                    .build();

        } else if (Type.VMC_AWS.equals(zone.getType())) {
            VmcAwsZone op = (VmcAwsZone) zone;
            Zone.Network n = op.getNetwork();

            IPv4Network network = IPv4Network.newBuilder()
                    .setName(n.getName())
                    .setAddressAllocation(IPv4Network.AddressAllocationScheme.STATIC)
                    .setGatewayIp(n.getGateway())
                    .setSubnet(Integer.parseInt(n.getSubnet()))
                    .setAllocationServer(Endpoint.newBuilder().build())
                    .addAllNameServers(n.getNameServers() == null ? Arrays.asList() : n.getNameServers())
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
                    .setCredential(toBearerCredential(op.getRefreshToken()))
                    .build();

            VmcOrchestrationSiteInfo vmcOrchestrationSiteInfo = VmcOrchestrationSiteInfo.newBuilder()
                    .setAuthentication(authenticationEndpoint)
                    .setApi(apiEndpoint)
                    .setOrganization(op.getOrganization())
                    .setDatacenter(op.getDatacenter())
                    .setVsphere(dcInfo)
                    .setWavefront(wavefront)
                    .setElasticsearch(elasticSearch)
                    .addAllLogManagements(toFleetLogManagements(op))
                    .build();
            return OrchestrationSiteInfo.newBuilder()
                    .setType(typeMap.get(zone.getType()))
                    .putAllLabels(toMap(zone))
                    .setVmc(vmcOrchestrationSiteInfo)
                    .build();
        }
        throw new BadRequestException("Invalid zone type {}", zone.getType());
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

    private static List<LogManagement> toFleetLogManagements(Zone zone) {

        List<LogManagement> logManagements = new ArrayList<>();
        if (Type.ON_PREM.equals(zone.getType())) {
            OnPremZone opZone = (OnPremZone) zone;
            if (opZone.getLogManagements() != null) {
                logManagements = opZone.getLogManagements().stream()
                        .map(lm -> LogManagement.newBuilder()
                                    .setDestinationValue(lm.getDestination().getValue())
                                    .setEndpoint(Endpoint.newBuilder()
                                        .setAddress(createAddress(lm.getAddress(), lm.getPort()))
                                        .setCredential(toPasswordCredential(lm.getUsername(), lm.getPassword()))
                                        .build())
                                    .setLogInsightAgentId(lm.getLogInsightAgentId())
                                    .build())
                        .collect(Collectors.toList());
            }
        }
        else if (Type.VMC_AWS.equals(zone.getType())) {
            VmcAwsZone vmcZone = (VmcAwsZone) zone;
            if (vmcZone.getLogManagements() != null) {
                logManagements = vmcZone.getLogManagements().stream()
                        .map(lm -> LogManagement.newBuilder()
                                    .setDestinationValue(lm.getDestination().getValue())
                                    .setEndpoint(Endpoint.newBuilder()
                                        .setAddress(createAddress(lm.getAddress(), lm.getPort()))
                                        .setCredential(toBearerCredential(lm.getToken()))
                                        .build())
                                    .build())
                        .collect(Collectors.toList());
            } else {
                // As of right now VMC AWS zones are not created from zones API
                // Add static logging information for VMC for now
                LogManagement vmcLogManagement = LogManagement.newBuilder()
                                                .setDestination(LogManagement.Type.LOG_INTELLIGENCE)
                                                .setEndpoint(Endpoint.newBuilder()
                                                    .setAddress(lintEndpoint)
                                                    .setCredential(toBearerCredential(lintAuthBearer))
                                                    .build())
                                                .build();
                logManagements.add(vmcLogManagement);
            }
        }

        return logManagements;
    }

    private static String createAddress(String address, int port) {
        return port == 0 ? address : address + ':' + port;
    }
}
