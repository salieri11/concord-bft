/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vm;

import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.UUID;

import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.util.ReflectionTestUtils;

import com.google.protobuf.ProtocolStringList;
import com.vmware.blockchain.deployment.server.Application;
import com.vmware.blockchain.deployment.services.exception.BadRequestPersephoneException;
import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorData.CreateComputeResourceRequestV2;
import com.vmware.blockchain.deployment.services.util.password.PasswordGeneratorUtil;
import com.vmware.blockchain.deployment.v1.ConcordAgentConfiguration;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.Credential;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.IPv4Network;
import com.vmware.blockchain.deployment.v1.OutboundProxyInfo;
import com.vmware.blockchain.deployment.v1.PasswordCredential;
import com.vmware.blockchain.deployment.v1.VSphereDatacenterInfo;

/**
 * Tests for the class that generates User Data.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:bootstrap-test.properties")
@ContextConfiguration(classes = {Application.class})
public class CloudInitConfigurationTest {

    CloudInitConfiguration cloudInitConfiguration;

    @Mock
    private Endpoint containerRegistry;

    @Mock
    private Endpoint notaryServer;

    @Mock
    private ConcordModelSpecification model;

    @Mock
    private ProtocolStringList nameServers;

    @Mock
    private ConfigurationSessionIdentifier configGenId;

    @Mock
    private Endpoint configServiceRestEndpoint;

    @Mock
    private OutboundProxyInfo outboundProxy;

    @Mock
    private IPv4Network network;

    @Mock
    CreateComputeResourceRequestV2 request;

    @Mock
    VSphereDatacenterInfo datacenterInfo;

    /**
     * Initialize various mocks.
     */
    @BeforeEach
    void init() {
        setupRequestObject();
        setupDatacenterInfoObject();

        cloudInitConfiguration = new CloudInitConfiguration(request, datacenterInfo, "c0nc0rd");
        cloudInitConfiguration = spy(cloudInitConfiguration);

        doReturn(ConcordAgentConfiguration.getDefaultInstance()).when(cloudInitConfiguration).getConfiguration();
    }

    private void setupRequestObject() {

        // mock containerRegistry details
        Credential mockCredential = mock(Credential.class);
        when(mockCredential.getType()).thenReturn(Credential.Type.PASSWORD);
        when(mockCredential.getPasswordCredential()).thenReturn(PasswordCredential.newBuilder()
                .setPassword("password")
                .setUsername("username").build());
        when(containerRegistry.getCredential()).thenReturn(mockCredential);
        when(containerRegistry.getAddress()).thenReturn("https://containerRegistry.com");

        // mock model details
        when(model.getComponentsList()).thenReturn(Arrays.asList(ConcordComponent.newBuilder()
                .setType(ConcordComponent.Type.CONTAINER_IMAGE)
                .setServiceType(ConcordComponent.ServiceType.GENERIC)
                .setName("agent")
                .build()
        ));

        // mock cloudInitData details
        CreateComputeResourceRequestV2.CloudInitData cloudInitData =
                mock(CreateComputeResourceRequestV2.CloudInitData.class);
        when(cloudInitData.getContainerRegistry()).thenReturn(containerRegistry);
        when(cloudInitData.getModel()).thenReturn(model);
        when(cloudInitData.getPrivateIp()).thenReturn("10.0.0.10");
        when(cloudInitData.getConfigGenId()).thenReturn(configGenId);
        when(cloudInitData.getConfigServiceRestEndpoint()).thenReturn(configServiceRestEndpoint);

        // mock request details
        UUID nodeId = UUID.fromString("5f3e3221-9c92-4d92-b328-67d08ac91a81");
        UUID blockchainId = UUID.fromString("1ddf42e1-29ed-4d36-8e07-5f545d16c15c");
        when(request.getCloudInitData()).thenReturn(cloudInitData);
        when(request.getNodeId()).thenReturn(nodeId);
        when(request.getBlockchainId()).thenReturn(blockchainId);
    }

    private void setupDatacenterInfoObject() {

        // mock network details
        when(network.getNameServersList()).thenReturn(nameServers);
        when(network.getSubnet()).thenReturn(24);
        when(network.getGatewayIp()).thenReturn("10.0.0.1");

        // mock datacenterInfo details
        when(datacenterInfo.getNetwork()).thenReturn(network);
        when(datacenterInfo.getOutboundProxy()).thenReturn(outboundProxy);
    }

    @Test
    void testDefault() throws IOException {
        String output = cloudInitConfiguration.userData();
        Assert.assertNotNull(output);
        File file = new File(getClass().getClassLoader().getResource("userdata/default-user-data.txt").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        Assert.assertEquals(expected, output);
    }

    @Test
    void testException() {
        when(containerRegistry.getCredential()).thenReturn(null);
        try {
            cloudInitConfiguration.userData();
        } catch (Exception e) {
            Assert.assertTrue(e instanceof PersephoneException);
            Assert.assertEquals("Error generating user-data", e.getLocalizedMessage());
        }
    }

    @Test
    void testDefaultWithProxy() throws IOException {
        when(outboundProxy.getHttpHost()).thenReturn("http://outbound");
        when(outboundProxy.getHttpsHost()).thenReturn("https://outbound");
        when(outboundProxy.getHttpPort()).thenReturn(1234);
        when(outboundProxy.getHttpsPort()).thenReturn(12345);

        String output = cloudInitConfiguration.userData();
        Assert.assertNotNull(output);

        File file = new File(getClass().getClassLoader().getResource("userdata/user-data-with-proxy.txt").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        Assert.assertEquals(expected, output);
    }

    @Test
    void testInsecureRegistry() throws IOException {
        when(containerRegistry.getAddress()).thenReturn("http://containerRegistry.com");
        when(nameServers.isEmpty()).thenReturn(true);

        String output = cloudInitConfiguration.userData();
        Assert.assertNotNull(output);

        File file = new File(
                getClass().getClassLoader().getResource("userdata/user-data-with-insecure-registry.txt").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        Assert.assertEquals(expected, output);
    }

    // Test to check if the environment variables for notary verification for agent image are set correctly
    // This test handles the case where notary server is provided
    // All other tests inherently test the scenario of notary server not being provided
    @Test
    void testNotaryServer() throws IOException {
        when(notaryServer.getAddress()).thenReturn("\"https://notary.example.com\"");

        // mock cloudInitData details with above change to notaryServer Address
        CreateComputeResourceRequestV2.CloudInitData cloudInitData =
                mock(CreateComputeResourceRequestV2.CloudInitData.class);
        when(cloudInitData.getContainerRegistry()).thenReturn(containerRegistry);
        when(cloudInitData.getNotaryServer()).thenReturn(notaryServer);
        when(cloudInitData.getModel()).thenReturn(model);
        when(cloudInitData.getPrivateIp()).thenReturn("10.0.0.10");
        when(cloudInitData.getConfigGenId()).thenReturn(configGenId);
        when(cloudInitData.getConfigServiceRestEndpoint()).thenReturn(configServiceRestEndpoint);
        when(request.getCloudInitData()).thenReturn(cloudInitData);

        cloudInitConfiguration = new CloudInitConfiguration(request, datacenterInfo, "c0nc0rd");
        cloudInitConfiguration = spy(cloudInitConfiguration);
        doReturn(ConcordAgentConfiguration.getDefaultInstance()).when(cloudInitConfiguration).getConfiguration();

        String output = cloudInitConfiguration.userData();
        Assert.assertNotNull(output);

        File file = new File(
                getClass().getClassLoader().getResource("userdata/user-data-with-notary-server.txt").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        Assert.assertEquals(expected, output);
    }

    @Test
    void testConcordAgentConfig() {
        doCallRealMethod().when(cloudInitConfiguration).getConfiguration();
        Assert.assertNotNull(cloudInitConfiguration.getConfiguration());
    }

    // Test to check if Agent Config contains notary server address, if notary server address is provided
    @Test
    void testConcordAgentConfigWithNotaryServer() {
        when(notaryServer.getAddress()).thenReturn("\"https://notary.new.example.com\"");
        // mock cloudInitData details with above change to notaryServer Address
        CreateComputeResourceRequestV2.CloudInitData cloudInitData =
                mock(CreateComputeResourceRequestV2.CloudInitData.class);
        when(cloudInitData.getContainerRegistry()).thenReturn(containerRegistry);
        when(cloudInitData.getNotaryServer()).thenReturn(notaryServer);
        when(cloudInitData.getModel()).thenReturn(model);
        when(cloudInitData.getPrivateIp()).thenReturn("10.0.0.10");
        when(cloudInitData.getConfigGenId()).thenReturn(configGenId);
        when(cloudInitData.getConfigServiceRestEndpoint()).thenReturn(configServiceRestEndpoint);
        when(request.getCloudInitData()).thenReturn(cloudInitData);

        cloudInitConfiguration = new CloudInitConfiguration(request, datacenterInfo, "c0nc0rd");
        cloudInitConfiguration = spy(cloudInitConfiguration);

        doCallRealMethod().when(cloudInitConfiguration).getConfiguration();
        Assert.assertEquals(cloudInitConfiguration.getConfiguration().getNotaryServer().getAddress(), "\"https://notary.new.example.com\"");
        Assert.assertNotNull(cloudInitConfiguration.getConfiguration());
    }

    // Test to check if Agent Config contains empty notary server address, if notary server address is not provided
    @Test
    void testConcordAgentConfigWithoutNotaryServer() {
        cloudInitConfiguration = new CloudInitConfiguration(request, datacenterInfo, "c0nc0rd");
        cloudInitConfiguration = spy(cloudInitConfiguration);

        doCallRealMethod().when(cloudInitConfiguration).getConfiguration();
        Assert.assertEquals("", cloudInitConfiguration.getConfiguration().getNotaryServer().getAddress());
        Assert.assertNotNull(cloudInitConfiguration.getConfiguration());
    }

    @Test
    void vmGeneratedPassword() {
        String vmGeneratedPassword = PasswordGeneratorUtil.generateCommonTextPassword();
        ReflectionTestUtils.setField(cloudInitConfiguration, "vmPassword", vmGeneratedPassword);
        String output = cloudInitConfiguration.userData();
        Assert.assertNotNull(output);

        String expectedPasswdLine = "echo -e \"{{vmPassword}}\\n{{vmPassword}}\" | /bin/passwd"
                                    .replace("{{vmPassword}}", vmGeneratedPassword);
        String actualPasswdLine = output.split("\\r?\\n")[1];

        Assert.assertEquals(expectedPasswdLine, actualPasswdLine);
    }

    @Test
    void vmAddDisk() {
        ReflectionTestUtils.setField(cloudInitConfiguration, "newDisk", true);
        String[] userDataLines = cloudInitConfiguration.userData().split("\\r?\\n");
        Assert.assertNotNull(userDataLines);

        String commentDiskCmd = "# Partition, format, and mount additional disk, if any";
        String expectedDiskCmd = "parted -s -a optimal /dev/sdb mklabel gpt -- mkpart primary ext4 0% 100%;sleep 2;"
                + "mkfs.ext4 /dev/sdb1;sleep 5;mkdir /mnt/data;mount /dev/sdb1 /mnt/data;"
                + "echo -e \"`blkid /dev/sdb1 | cut -d\" \" -f4` /mnt/data ext4 defaults 0 0\" >> /etc/fstab;";

        String actualDiskCmd = null;
        for (int i = 0; i < userDataLines.length; ++i) {
            if (userDataLines[i].equals(commentDiskCmd)) {
                if (i + 1 < userDataLines.length) {
                    actualDiskCmd = userDataLines[i + 1];
                }
                break;
            }
        }

        Assert.assertNotNull(actualDiskCmd);
        Assert.assertEquals(expectedDiskCmd, actualDiskCmd);
    }

    @Nested
    class TestGateway {

        @Test
        void badGateway() {
            setupRequestObject();
            String badGateway = "10.0.1";

            // mock network details
            when(network.getNameServersList()).thenReturn(nameServers);
            when(network.getSubnet()).thenReturn(24);
            when(network.getGatewayIp()).thenReturn(badGateway);

            // mock datacenterInfo details
            when(datacenterInfo.getNetwork()).thenReturn(network);
            when(datacenterInfo.getOutboundProxy()).thenReturn(outboundProxy);

            try {
                cloudInitConfiguration = new CloudInitConfiguration(request, datacenterInfo, "c0nc0rd");
            } catch (Exception e) {
                Assert.assertTrue(e instanceof BadRequestPersephoneException);
                Assert.assertEquals(String.format("Provided gateway %s not a valid Inet address", badGateway),
                        e.getLocalizedMessage());
            }
        }

    }

}
