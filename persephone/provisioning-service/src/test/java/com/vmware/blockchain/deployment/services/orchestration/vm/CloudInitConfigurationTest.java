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
import java.util.List;

import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.util.ReflectionTestUtils;

import com.vmware.blockchain.deployment.server.Application;
import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.services.util.password.PasswordGeneratorUtil;
import com.vmware.blockchain.deployment.v1.ConcordAgentConfiguration;
import com.vmware.blockchain.deployment.v1.ConcordClusterIdentifier;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.Credential;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.OutboundProxyInfo;
import com.vmware.blockchain.deployment.v1.PasswordCredential;

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
    private ConcordModelSpecification model;

    private String ipAddress = "10.0.0.10";

    private String gateway = "10.0.0.1";

    @Mock
    private List<String> nameServers;

    private Integer subnet = 24;

    @Mock
    private ConcordClusterIdentifier clusterId;

    private String nodeIdString = "12345";

    @Mock
    private ConfigurationSessionIdentifier configGenId;

    @Mock
    private Endpoint configServiceRestEndpoint;

    @Mock
    private OutboundProxyInfo outboundProxy;

    /**
     * Initialize various mocks.
     */
    @BeforeEach
    void init() {
        cloudInitConfiguration =
                new CloudInitConfiguration(containerRegistry, model, ipAddress, gateway, nameServers, subnet, clusterId,
                                           nodeIdString, configGenId,
                                           configServiceRestEndpoint, outboundProxy, "c0nc0rd");
        cloudInitConfiguration = spy(cloudInitConfiguration);

        Credential mockCredential = mock(Credential.class);
        when(mockCredential.getType()).thenReturn(Credential.Type.PASSWORD);
        when(mockCredential.getPasswordCredential()).thenReturn(PasswordCredential.newBuilder()
                                                                        .setPassword("password")
                                                                        .setUsername("username").build());
        when(containerRegistry.getCredential()).thenReturn(mockCredential);
        when(containerRegistry.getAddress()).thenReturn("https://containerRegistry.com");

        when(model.getComponentsList()).thenReturn(Arrays.asList(ConcordComponent.newBuilder()
                                                                         .setType(ConcordComponent.Type.CONTAINER_IMAGE)
                                                                         .setServiceType(
                                                                                 ConcordComponent.ServiceType.GENERIC)
                                                                         .setName("agent")
                                                                         .build()
        ));

        doReturn(ConcordAgentConfiguration.getDefaultInstance()).when(cloudInitConfiguration).getConfiguration();
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

    @Test
    void testConcordAgentConfig() {
        doCallRealMethod().when(cloudInitConfiguration).getConfiguration();
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

}
