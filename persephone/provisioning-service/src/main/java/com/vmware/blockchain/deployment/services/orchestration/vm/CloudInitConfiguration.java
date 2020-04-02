/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vm;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
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
import com.vmware.blockchain.deployment.v1.Wavefront;

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
    private Wavefront wavefront;

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
                                  OutboundProxyInfo outboundProxy,
                                  Wavefront wavefront) {
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
        this.wavefront = wavefront;
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
        return "echo -e \"[Match]\\nName=eth0\\n\\n[Network]\\nAddress=" + ipAddress + "/" + subnet
               + "\\nGateway=" + gateway + " " + dnsEntry + "\" > /etc/systemd/network/10-eth0-static.network; "
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
     * Retrieve the user-data manifest of the Cloud-Init configuration.
     *
     * @return user-data manifest as a [String].
     */
    public String userData1() {

        // FIXME: Use the relative path.
        File fileToBeModified = new File("classpath:/CloudInit.txt");
        BufferedReader reader = null;
        FileWriter writer = null;

        String oldContent = "";
        try {
            reader = new BufferedReader(new FileReader(fileToBeModified));
            //Reading all the lines of input text file into oldContent
            String line = reader.readLine();

            while (line != null) {
                oldContent = oldContent + line + System.lineSeparator();
                line = reader.readLine();
                switch (line) {
                    case "{{dockerLoginCommand}}":
                        line = toRegistryLoginCommand(containerRegistry);
                        break;
                    case "{{registrySecuritySetting}}":
                        line = toRegistrySecuritySetting(containerRegistry.getAddress());
                        break;
                    case "{{agentImage}}":
                        line = URI.create(containerRegistry.getAddress()).getAuthority() + "/" + agentImageName();
                        break;
                    case "{{agentConfig}}":
                        line = com.google.protobuf.util.JsonFormat.printer().print(getConfiguration());
                        break;
                    case "{{networkSetupCommand}}":
                        line = networkSetupCommand();
                        break;
                    case "{{dockerDns}}":
                        line = dockerDnsSetupCommand();
                        break;
                    case "{{setupOutboundProxy}}":
                        line = setupOutboundProxy();
                        break;
                    default:
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                //Closing the resources
                reader.close();
                writer.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
            return oldContent;
        }
    }

    /**
     * User data initializer.
     * @return info.
     */
    public String userData() {

        try {

            String temp = "#!/bin/sh\n"
                          + "echo -e \"c0nc0rd\\nc0nc0rd\" | /bin/passwd\n"
                          + "\n"
                          + "{{networkSetupCommand}}\n"
                          + "\n"
                          + "sed -i 's_/usr/bin/dockerd.*_/usr/bin/dockerd {{dockerDns}} -H tcp://127.0.0.1:2375 "
                          + "-H unix:///var/run/docker.sock {{registrySecuritySetting}}_g'"
                          + " /lib/systemd/system/docker.service\n"
                          + "\n"
                          + "{{setupOutboundProxy}}\n"
                          + "systemctl daemon-reload\n"
                          + "\n"
                          + "mkdir -p /etc/docker\n"
                          + "echo '{\"log-driver\": \"json-file\", \"log-opts\": { \"max-size\": \"100m\","
                          + " \"max-file\": \"5\"}}' > /etc/docker/daemon.json\n"
                          + "\n"
                          + "systemctl restart docker\n"
                          + "\n"
                          + "# To enable docker on boot.\n"
                          + "systemctl enable docker\n"
                          + "\n"
                          + "# Enable time sync\n"
                          + "vmware-toolbox-cmd timesync enable\n"
                          + "\n"
                          + "{{dockerLoginCommand}}\n"
                          + "\n"
                          + "# Output the node's model specification.\n"
                          + "mkdir -p /config/agent\n"
                          + "echo '{{agentConfig}}' > /config/agent/config.json\n"
                          + "\n"
                          + "# Update guest-info's network information in vSphere.\n"
                          + "touch /etc/vmware-tools/tools.conf\n"
                          + "printf '[guestinfo]\\nprimary-nics=eth*\\nexclude-nics=docker*,veth*'"
                          + " > /etc/vmware-tools/tools.conf\n"
                          + "/usr/bin/vmware-toolbox-cmd info update network\n"
                          + "\n"
                          + "docker run -d --name=agent --restart=always -v /config:/config"
                          + " -v /var/run/docker.sock:/var/run/docker.sock -p 8546:8546 {{agentImage}}\n"
                          + "echo 'done'";

            temp = temp.replace("{{dockerLoginCommand}}", toRegistryLoginCommand(containerRegistry))
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

            return temp;
        } catch (Exception e) {
            throw new PersephoneException("Error generating user-data", e);
        }
    }
}