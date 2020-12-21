/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vm;

import java.io.InputStream;
import java.net.InetAddress;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.assertj.core.util.Strings;
import org.slf4j.MDC;
import org.springframework.util.StringUtils;

import com.google.common.net.InetAddresses;
import com.vmware.blockchain.deployment.common.Constants;
import com.vmware.blockchain.deployment.services.exception.BadRequestPersephoneException;
import com.vmware.blockchain.deployment.services.exception.ErrorCode;
import com.vmware.blockchain.deployment.services.exception.NotFoundPersephoneException;
import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorData;
import com.vmware.blockchain.deployment.v1.AgentAttributes;
import com.vmware.blockchain.deployment.v1.ConcordAgentConfiguration;
import com.vmware.blockchain.deployment.v1.ConcordClusterIdentifier;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.Credential;
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.NodeType;
import com.vmware.blockchain.deployment.v1.OutboundProxyInfo;
import com.vmware.blockchain.deployment.v1.Properties;
import com.vmware.blockchain.deployment.v1.TransportSecurity;
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
    private Endpoint notaryServer;
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
    private boolean mountNewDiskToPrimary;
    private NodeType nodeType;

    /** Container network name alias.
     * TODO: This could be unified across agent and provisioning service and sent via agent request.
     **/
    private static final String CONTAINER_NETWORK_NAME = "blockchain-fabric";

    /**
     * Constructor.
     */
    public CloudInitConfiguration(OrchestratorData.CreateComputeResourceRequestV2 request,
                                  VSphereDatacenterInfo datacenterInfo,
                                  String vmPassword) {

        this.containerRegistry = request.getCloudInitData().getContainerRegistry();
        if (request.getProperties().getOrDefault(
                DeploymentAttributes.NOTARY_VERIFICATION_ENABLED.name(), "false").equals("true")) {
            this.notaryServer = request.getCloudInitData().getNotaryServer();
        } else {
            this.notaryServer = Endpoint.newBuilder().build();
        }

        this.model = request.getCloudInitData().getModel();
        this.ipAddress = request.getCloudInitData().getPrivateIp();
        this.gateway = getGateway(datacenterInfo);
        this.nameServers = datacenterInfo.getNetwork().getNameServersList().isEmpty()
                           ? Arrays.asList("8.8.8.8", "8.8.4.4")
                           : datacenterInfo.getNetwork().getNameServersList();
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
        this.mountNewDiskToPrimary = Boolean.parseBoolean(request.getProperties()
                .getOrDefault(DeploymentAttributes.ADD_DISK_PRIMARY.name(), "False"));
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
        val dnsString = nameServers.stream().collect(Collectors.joining(" "));
        var dnsEntry = "\\nDNS=" + dnsString;
        return "echo -e \"[Match]\\nName=eth0"
               + "\\n\\n[Network]\\nAddress=" + ipAddress + "/" + subnet
               + "\\nGateway=" + gateway
               + dnsEntry
               + "\\nIPv6AcceptRA=false" + "\" > /etc/systemd/network/10-eth0-static.network; "
               + "chmod 644 /etc/systemd/network/10-eth0-static.network; systemctl restart systemd-networkd;";
    }

    private String dockerDnsSetupCommand() {
        val dnsString = nameServers.stream().collect(Collectors.joining(" --dns "));
        return "--dns " + dnsString;
    }

    private String setDockerContentTrustServerCommand() {
        if (notaryServer != null && !notaryServer.getAddress().equals("")) {
            log.info("Notary Server Endpoint received is " + this.notaryServer.getAddress());
            return "export DOCKER_CONTENT_TRUST_SERVER=" + this.notaryServer.getAddress();
        } else {
            log.info("Notary Server Address is not provided");
            return "export DOCKER_CONTENT_TRUST_SERVER=\"\"";
        }
    }

    private Boolean isNotaryServerSelfSigned() {
        if (notaryServer != null && !notaryServer.getAddress().equals("")
            && notaryServer.getTransportSecurity() != null
            && notaryServer.getTransportSecurity().getType() != TransportSecurity.Type.NONE
            && StringUtils.hasText(notaryServer.getTransportSecurity().getCertificateData())) {
            return true;
        }
        return false;
    }

    // Sets the directory name for Notary Self-Signed Cert as required by Docker
    // Format required is host:port
    private String setDirForNotarySelfSignedCert() {
        if (isNotaryServerSelfSigned()) {
            try {
                URL url = new URL(notaryServer.getAddress());
                String dirName = "";
                if (StringUtils.hasText(url.getHost())) {
                    dirName += url.getHost();
                } else {
                    throw new BadRequestPersephoneException(new MalformedURLException(),
                                                  ErrorCode.NOTARY_SERVER_ADDRESS_MALFORMED, notaryServer.getAddress());
                }
                if (url.getPort() != -1) {
                    dirName += ":";
                    dirName += url.getPort();
                }
                return dirName;
            } catch (Exception e) {
                throw new BadRequestPersephoneException(e, ErrorCode.NOTARY_SERVER_ADDRESS_MALFORMED,
                                                        notaryServer.getAddress());
            }
        } else {
            return "";
        }
    }

    private String handleSelfSignedNotaryServer() {
        String certCreateCmd = "";
        if (isNotaryServerSelfSigned()) {
            certCreateCmd = "mkdir -p ~/.docker/tls/" + setDirForNotarySelfSignedCert()
                            + "\necho '" + notaryServer.getTransportSecurity().getCertificateData()
                            + "' > ~/.docker/tls/" + setDirForNotarySelfSignedCert() + "/ca.crt"
                            + "\nchmod 600 ~/.docker/tls/" + setDirForNotarySelfSignedCert() + "/ca.crt"
                            + "\ncp ~/.docker/tls/" + setDirForNotarySelfSignedCert()
                            + "/ca.crt /config/agent/notarySelfSignedCert.crt";
        }
        return certCreateCmd;
    }

    private Boolean isContainerRegSelfSigned() {
        if (containerRegistry != null && !containerRegistry.getAddress().equals("")
            && containerRegistry.getTransportSecurity() != null
            && containerRegistry.getTransportSecurity().getType() != TransportSecurity.Type.NONE
            && StringUtils.hasText(containerRegistry.getTransportSecurity().getCertificateData())) {
            return true;
        }
        return false;
    }

    // Sets the directory name for Container Registry's Self-Signed Cert as required by Docker
    // Format required is host:port
    private String setDirForContainerRegSelfSignedCert() {
        if (isContainerRegSelfSigned()) {
            try {
                URL url = new URL(containerRegistry.getAddress());
                String dirName = "";
                if (StringUtils.hasText(url.getHost())) {
                    dirName += url.getHost();
                } else {
                    throw new BadRequestPersephoneException(new MalformedURLException(),
                                                            ErrorCode.CONTAINER_REG_ADDRESS_MALFORMED,
                                                            containerRegistry.getAddress());
                }
                if (url.getPort() != -1) {
                    dirName += ":";
                    dirName += url.getPort();
                }
                return dirName;
            } catch (Exception e) {
                throw new BadRequestPersephoneException(e, ErrorCode.CONTAINER_REG_ADDRESS_MALFORMED,
                                                        containerRegistry.getAddress());
            }
        } else {
            return "";
        }
    }

    private String handleSelfSignedContainerReg() {
        String certCreateCmd = "";
        if (isContainerRegSelfSigned()) {
            certCreateCmd = "mkdir -p /etc/docker/certs.d/" + setDirForContainerRegSelfSignedCert()
                            + "\necho '" + containerRegistry.getTransportSecurity().getCertificateData()
                            + "' > /etc/docker/certs.d/" + setDirForContainerRegSelfSignedCert() + "/ca.crt"
                            + "\nchmod 600 /etc/docker/certs.d/" + setDirForContainerRegSelfSignedCert() + "/ca.crt";
        }
        return certCreateCmd;
    }

    private String enableDockerContentTrustCommand() {
        if (notaryServer != null && !notaryServer.getAddress().equals("")) {
            return "export DOCKER_CONTENT_TRUST=1";
        } else {
            return "export DOCKER_CONTENT_TRUST=0";
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
        Properties.Builder propBuilder = Properties.newBuilder();
        if (noLaunch) {
            propBuilder.putValues(AgentAttributes.COMPONENT_NO_LAUNCH.name(), "True");
        }
        if (newDisk && !mountNewDiskToPrimary) {
            // Set NEW_DATA_DISK in Agent if adding a new disk to its own partition
            propBuilder.putValues(AgentAttributes.NEW_DATA_DISK.name(), "True");
        }
        var builder = ConcordAgentConfiguration.newBuilder()
                .setCluster(clusterId)
                .setModel(model)
                .setConfigService(configServiceRestEndpoint)
                .setConfigurationSession(configGenId)
                .setOutboundProxyInfo(outboundProxy)
                .setOpid(MDC.get(Constants.OPID))
                .setProperties(propBuilder);

        if (!Strings.isNullOrEmpty(nodeIdString)) {
            builder.setNodeId(nodeIdString);
        }
        if (this.containerRegistry != null) {
            // Clearing TransportSecurity of ContainerRegistry as Cloud Init creates and handles the required cert file
            if (isContainerRegSelfSigned()) {
                builder = builder.setContainerRegistry(containerRegistry.toBuilder().clearTransportSecurity());
            } else {
                builder = builder.setContainerRegistry(containerRegistry);
            }
        }
        if (this.notaryServer != null) {
            // Clearing TransportSecurity of NotaryServer as Cloud Init creates and handles the required cert file
            if (notaryServer.getTransportSecurity() != null) {
                builder = builder.setNotaryServer(notaryServer.toBuilder().clearTransportSecurity());
            } else {
                builder = builder.setNotaryServer(notaryServer);
            }
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
            if (mountNewDiskToPrimary) {
                log.info("Adding new disk to existing LVM and mountpoint '/'");
                diskCmd = "parted -s -a optimal /dev/sdb mklabel gpt -- mkpart primary ext4 0% 100% set 1 lvm on;"
                        + "sleep 2;"
                        + "pvcreate /dev/sdb1; vgextend vg0 /dev/sdb1; lvextend /dev/vg0/data /dev/sdb1; "
                        + "resize2fs /dev/vg0/data;";
            }
            else {
                String mountDir = "/mnt/data";
                log.info(String.format("Adding new disk in a separate partition and mountpoint '%s'", mountDir));
                diskCmd = "parted -s -a optimal /dev/sdb mklabel gpt -- mkpart primary ext4 0% 100%;"
                        + "sleep 2;"
                        + "mkfs.ext4 /dev/sdb1;"
                        + "sleep 5;"
                        + "mkdir " + mountDir
                        + ";mount /dev/sdb1 " + mountDir
                        + ";echo -e \"`blkid /dev/sdb1 | cut -d\" \" -f4` " + mountDir
                        + " ext4 defaults 0 0\" >> /etc/fstab;";
            }
        }
        return diskCmd;
    }

    private String coreDumpCommand() {
        if (model.getNodeType() == ConcordModelSpecification.NodeType.DAML_COMMITTER
            || model.getNodeType() == ConcordModelSpecification.NodeType.CONCORD) {
            return "sysctl kernel.core_pattern=/concord/cores/core.%e.%h.%s.%t > /dev/null";
        }
        return "";
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
                    .replace("{{setDockerContentTrustServerCommand}}", setDockerContentTrustServerCommand())
                    .replace("{{enableDockerContentTrustCommand}}", enableDockerContentTrustCommand())
                    .replace("{{agentImage}}",
                             URI.create(containerRegistry.getAddress()).getAuthority()
                             + "/" + agentImageName())
                    .replace("{{registrySecuritySetting}}",
                             toRegistrySecuritySetting(containerRegistry.getAddress()))
                    .replace("{{agentConfig}}",
                             com.google.protobuf.util.JsonFormat.printer().print(getConfiguration()))
                    .replace("{{handleSelfSignedContainerReg}}", handleSelfSignedContainerReg())
                    .replace("{{handleSelfSignedNotaryServer}}", handleSelfSignedNotaryServer())
                    .replace("{{networkSetupCommand}}", networkSetupCommand())
                    .replace("{{dockerDns}}", dockerDnsSetupCommand())
                    .replace("{{setupOutboundProxy}}", setupOutboundProxy())
                    .replace("{{blockchainNetwork}}", CONTAINER_NETWORK_NAME)
                    .replace("{{diskSetupCommand}}", diskSetupCommand())
                    .replace("{{coreDumpCommand}}", coreDumpCommand());

            return content;
        } catch (Exception e) {
            throw new PersephoneException(e, "Error generating user-data");
        }
    }
}
