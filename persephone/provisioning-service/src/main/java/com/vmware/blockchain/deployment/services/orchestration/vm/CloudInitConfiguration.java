/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vm;

import java.io.FileInputStream;
import java.io.InputStream;
import java.net.URI;
import java.util.List;
import java.util.stream.Collectors;

import org.assertj.core.util.Strings;

import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.v1.ConcordAgentConfiguration;
import com.vmware.blockchain.deployment.v1.ConcordClusterIdentifier;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.Credential;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.OutboundProxyInfo;

import lombok.extern.slf4j.Slf4j;
import lombok.val;

/**
 * Initialization script run either on first-boot of a deployed virtual machine.
 */
@Slf4j
public class CloudInitConfiguration {

    private Endpoint containerRegistry;
    private ConcordModelSpecification model;
    private String ipAddress;
    private String gateway;
    private List<String> nameServers;
    private int subnet;
    private ConcordClusterIdentifier clusterId;
    private int nodeId;
    private ConfigurationSessionIdentifier configGenId;
    private Endpoint configServiceEndpoint;
    private Endpoint configServiceRestEndpoint;
    private OutboundProxyInfo outboundProxy;

    /**
     * Constructor.
     */
    public CloudInitConfiguration(Endpoint containerRegistry,
                                  ConcordModelSpecification model,
                                  String ipAddress,
                                  String gateway,
                                  List<String> nameServers,
                                  int subnet,
                                  ConcordClusterIdentifier clusterId,
                                  int nodeId,
                                  ConfigurationSessionIdentifier configGenId,
                                  Endpoint configServiceEndpoint,
                                  Endpoint configServiceRestEndpoint,
                                  OutboundProxyInfo outboundProxy) {
        this.containerRegistry = containerRegistry;
        this.model = model;
        this.ipAddress = ipAddress;
        this.gateway = gateway;
        this.nameServers = nameServers;
        this.subnet = subnet;
        this.clusterId = clusterId;
        this.nodeId = nodeId;
        this.configGenId = configGenId;
        this.configServiceEndpoint = configServiceEndpoint;
        this.configServiceRestEndpoint = configServiceRestEndpoint;
        this.outboundProxy = outboundProxy;
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
    protected ConcordAgentConfiguration getConfiguration() {
        return ConcordAgentConfiguration.newBuilder()
                .setContainerRegistry(containerRegistry)
                .setCluster(clusterId)
                .setNode(nodeId)
                .setModel(model)
                .setConfigService(outboundProxy == null || Strings.isNullOrEmpty(outboundProxy.getHttpsHost())
                                  ? configServiceEndpoint : configServiceRestEndpoint)
                .setConfigurationSession(configGenId)
                .setOutboundProxyInfo(outboundProxy)
                .build();
    }

    /**
     * Convert an endpoint to a Docker Registry login command.
     *
     * @return docker login command as a [String].
     */
    private String toRegistryLoginCommand(Endpoint credential) {
        String passwordCredential = "";
        if (credential.getCredential().getType() == Credential.Type.PASSWORD) {
            passwordCredential = "-u " + credential.getCredential().getPasswordCredential().getUsername() + " -p '"
                                 + credential.getCredential().getPasswordCredential().getPassword() + "'";
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
     * User data initializer.
     * @return info.
     */
    public String userData() {

        try {
            InputStream inputStream = new FileInputStream("user-data.txt");
            String content = new String(inputStream.readAllBytes());

            content = content.replace("{{dockerLoginCommand}}", toRegistryLoginCommand(containerRegistry))
                    .replace("{{agentImage}}",
                             URI.create(containerRegistry.getAddress()).getAuthority()
                             + "/" + agentImageName())
                    .replace("{{registrySecuritySetting}}",
                             toRegistrySecuritySetting(containerRegistry.getAddress()))
                    .replace("{{agentConfig}}",
                             com.google.protobuf.util.JsonFormat.printer().print(getConfiguration()))
                    .replace("{{networkSetupCommand}}", networkSetupCommand())
                    .replace("{{dockerDns}}", dockerDnsSetupCommand())
                    .replace("{{setupOutboundProxy}}", setupOutboundProxy());

            return content;
        } catch (Exception e) {
            throw new PersephoneException("Error generating user-data", e);
        }
    }
}
