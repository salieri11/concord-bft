/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vm;

import java.io.InputStream;
import java.net.InetAddress;
import java.net.URI;
import java.net.UnknownHostException;
import java.util.List;
import java.util.stream.Collectors;

import org.assertj.core.util.Strings;

import com.google.common.net.InetAddresses;
import com.vmware.blockchain.deployment.services.exception.BadRequestPersephoneException;
import com.vmware.blockchain.deployment.services.exception.ErrorCode;
import com.vmware.blockchain.deployment.services.exception.NotFoundPersephoneException;
import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorData;
import com.vmware.blockchain.deployment.v1.ConcordAgentConfiguration;
import com.vmware.blockchain.deployment.v1.ConcordClusterIdentifier;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.Credential;
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.OutboundProxyInfo;
import com.vmware.blockchain.deployment.v1.VSphereDatacenterInfo;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import lombok.val;

/**
 * Initialization script run either on first-boot of a deployed virtual machine.
 */
@Slf4j
@Data
public class CloudInitConfiguration {

    private Endpoint containerRegistry;
    private ConcordModelSpecification model;
    private String ipAddress;
    private String gateway;
    private List<String> nameServers;
    private int subnet;
    private ConcordClusterIdentifier clusterId;
    private String nodeIdString;
    private ConfigurationSessionIdentifier configGenId;
    private Endpoint configServiceRestEndpoint;
    private OutboundProxyInfo outboundProxy;
    private String vmPassword;
    private boolean noLaunch;
    private boolean newDisk;

    /** Container network name alias.
     * TODO: This could be unified across agent and provisioning service and sent via agent request.
     **/
    private static final String CONTAINER_NETWORK_NAME = "blockchain-fabric";

    /**
     * Constructor.
     */
    @Deprecated
    public CloudInitConfiguration(Endpoint containerRegistry,
                                  ConcordModelSpecification model,
                                  String ipAddress,
                                  String gateway,
                                  List<String> nameServers,
                                  int subnet,
                                  ConcordClusterIdentifier clusterId,
                                  String nodeIdString,
                                  ConfigurationSessionIdentifier configGenId,
                                  Endpoint configServiceRestEndpoint,
                                  OutboundProxyInfo outboundProxy,
                                  String vmPassword,
                                  boolean noLaunch,
                                  boolean newDisk) {
        this.containerRegistry = containerRegistry;
        this.model = model;
        this.ipAddress = ipAddress;
        this.gateway = gateway;
        this.nameServers = nameServers;
        this.subnet = subnet;
        this.clusterId = clusterId;
        this.nodeIdString = nodeIdString;
        this.configGenId = configGenId;
        this.configServiceRestEndpoint = configServiceRestEndpoint;
        this.outboundProxy = outboundProxy;
        this.vmPassword = vmPassword;
        this.noLaunch = noLaunch;
        this.newDisk = newDisk;
    }

    /**
     * Constructor.
     */
    public CloudInitConfiguration(OrchestratorData.CreateComputeResourceRequestV2 request,
                                  VSphereDatacenterInfo datacenterInfo,
                                  String vmPassword) {

        this.containerRegistry = request.getCloudInitData().getContainerRegistry();
        this.model = request.getCloudInitData().getModel();
        this.ipAddress = request.getCloudInitData().getPrivateIp();
        this.gateway = getGateway(datacenterInfo);
        this.nameServers = datacenterInfo.getNetwork().getNameServersList();
        this.subnet = datacenterInfo.getNetwork().getSubnet();
        this.clusterId = ConcordClusterIdentifier.newBuilder()
                .setId(request.getBlockchainId().toString())
                .build();
        this.nodeIdString = request.getNodeId().toString();
        this.configGenId = request.getCloudInitData().getConfigGenId();
        this.configServiceRestEndpoint = request.getCloudInitData().getConfigServiceRestEndpoint();
        this.outboundProxy = datacenterInfo.getOutboundProxy();
        this.vmPassword = vmPassword;
        this.noLaunch = request.getProperties().containsKey(DeploymentAttributes.NO_LAUNCH.name());
        this.newDisk = request.getProperties().containsKey(DeploymentAttributes.VM_STORAGE.name());
    }

    private String getGateway(VSphereDatacenterInfo datacenterInfo) {
        String gateway;
        try {
            // TODO : deprecate gateway in favor of gatewayIp.
            gateway = datacenterInfo.getNetwork().getGatewayIp().isEmpty()
                    ? InetAddress.getByName(String.valueOf(datacenterInfo.getNetwork().getGateway())).getHostAddress()
                    : datacenterInfo.getNetwork().getGatewayIp();
        } catch (UnknownHostException e) {
            throw new NotFoundPersephoneException(e, ErrorCode.UNKNOWN_GATEWAY,
                    datacenterInfo.getNetwork().getGateway());
        }
        if (!InetAddresses.isInetAddress(gateway)) {
            throw new BadRequestPersephoneException(ErrorCode.SITE_GATEWAY_IP_INCORRECT, gateway);
        }
        return gateway;
    }

    private String agentImageName() {
        return model.getComponentsList().stream()
                .filter(k -> k.getType() == ConcordComponent.Type.CONTAINER_IMAGE)
                .filter(k -> k.getServiceType() == ConcordComponent.ServiceType.GENERIC)
                .findFirst().get().getName();
    }

    private String networkSetupCommand() {
        var dnsEntry = "";
        if (nameServers != null && !nameServers.isEmpty()) {
            val dnsString = nameServers.stream().collect(Collectors.joining(" "));
            dnsEntry = "\\nDNS=" + dnsString;
        }
        return "echo -e \"[Match]\\nName=eth0"
               + "\\n\\n[Network]\\nAddress=" + ipAddress + "/" + subnet
               + "\\nGateway=" + gateway
               + dnsEntry
               + "\\nIPv6AcceptRA=false" + "\" > /etc/systemd/network/10-eth0-static.network; "
               + "chmod 644 /etc/systemd/network/10-eth0-static.network; systemctl restart systemd-networkd;";
    }

    private String dockerDnsSetupCommand() {
        if (nameServers != null && !nameServers.isEmpty()) {
            val dnsString = nameServers.stream().collect(Collectors.joining(" --dns "));
            return "--dns " + dnsString;
        } else {
            return "";
        }
    }

    private String setupOutboundProxy() {
        if (outboundProxy != null && !Strings.isNullOrEmpty(outboundProxy.getHttpsHost())) {
            return "export http_proxy=" + outboundProxy.getHttpHost() + ":" + outboundProxy.getHttpPort()
                   + ";export https_proxy=" + outboundProxy.getHttpsHost() + ":" + outboundProxy.getHttpsPort()
                   + ";mkdir /etc/systemd/system/docker.service.d"
                   + ";echo -e '[Service]\\nEnvironment=\"HTTP_PROXY=http://" + outboundProxy.getHttpHost() + ":"
                   + outboundProxy.getHttpPort() + "\"\\nEnvironment=\"HTTPS_PROXY=" + outboundProxy.getHttpHost() + ":"
                   + outboundProxy.getHttpsPort() + "\"\\nEnvironment=\"NO_PROXY=localhost,127.0.0.1\"' "
                   + "> /etc/systemd/system/docker.service.d/http-proxy.conf";
        } else {
            return "";
        }
    }

    /**
     * Concord agent startup configuration parameters.
     */
    ConcordAgentConfiguration getConfiguration() {
        var builder = ConcordAgentConfiguration.newBuilder()
                .setContainerRegistry(containerRegistry)
                .setCluster(clusterId)
                .setModel(model)
                .setConfigService(configServiceRestEndpoint)
                .setConfigurationSession(configGenId)
                .setOutboundProxyInfo(outboundProxy);

        if (!Strings.isNullOrEmpty(nodeIdString)) {
            builder.setNodeId(nodeIdString);
        }
        if (noLaunch) {
            builder.setNoLaunch(true);
        }
        return builder.build();
    }

    /**
     * Convert an endpoint to a Docker Registry login command.
     *
     * @return docker login command as a [String].
     */
    private String toRegistryLoginCommand(Endpoint credential) {
        String passwordCredential = "";
        Credential passwordCred = credential.getCredential();
        if (passwordCred.getType() == Credential.Type.PASSWORD) {
            passwordCredential = "-u " + passwordCred.getPasswordCredential().getUsername() + " -p '"
                                 + passwordCred.getPasswordCredential().getPassword() + "'";
        }
        return "docker login " + credential.getAddress() + " " + passwordCredential;
    }

    /**
     * Return appropriate container registry setting for Docker daemon depending on the registry's URL scheme.
     *
     * @return an option [String] to be passed to Docker engine startup.
     */
    private String toRegistrySecuritySetting(String address) {
        val endpoint = URI.create(address);

        if (endpoint.getScheme().toLowerCase().equals("http")) {
            return "--insecure-registry " + endpoint.getAuthority();
        } else {
            return "";
        }
    }

    /**
     * Partitions, formats, and mounts second disk.
     *
     * @return a [String] containing all the commands to be run to setup second disk if need be.
     */
    private String diskSetupCommand() {
        String diskCmd = "";
        if (newDisk) {
            String mountDir = "/mnt/sdb";
            diskCmd = "parted -s -a optimal /dev/sdb mklabel gpt -- mkpart primary ext4 0% 100%;"
                    + "sleep 2;"
                    + "mkfs.ext4 /dev/sdb1;"
                    + "sleep 5;"
                    + "mkdir " + mountDir
                    + ";mount /dev/sdb1 " + mountDir
                    + ";echo -e \"`blkid /dev/sdb1 | cut -d\" \" -f4` " + mountDir
                    + " ext4 defaults 0 0\" >> /etc/fstab;";
        }
        return diskCmd;
    }

    /**
     * User data initializer.
     * @return info.
     */
    public String userData() {

        try {
            InputStream inputStream = getClass().getClassLoader().getResourceAsStream("user-data.txt");
            String content = new String(inputStream.readAllBytes());

            content = content.replace("{{vmPassword}}", this.vmPassword)
                    .replace("{{dockerLoginCommand}}", toRegistryLoginCommand(containerRegistry))
                    .replace("{{agentImage}}",
                             URI.create(containerRegistry.getAddress()).getAuthority()
                             + "/" + agentImageName())
                    .replace("{{registrySecuritySetting}}",
                             toRegistrySecuritySetting(containerRegistry.getAddress()))
                    .replace("{{agentConfig}}",
                             com.google.protobuf.util.JsonFormat.printer().print(getConfiguration()))
                    .replace("{{networkSetupCommand}}", networkSetupCommand())
                    .replace("{{dockerDns}}", dockerDnsSetupCommand())
                    .replace("{{setupOutboundProxy}}", setupOutboundProxy())
                    .replace("{{blockchainNetwork}}", CONTAINER_NETWORK_NAME)
                    .replace("{{diskSetupCommand}}", diskSetupCommand());

            return content;
        } catch (Exception e) {
            throw new PersephoneException(e, "Error generating user-data");
        }
    }
}
