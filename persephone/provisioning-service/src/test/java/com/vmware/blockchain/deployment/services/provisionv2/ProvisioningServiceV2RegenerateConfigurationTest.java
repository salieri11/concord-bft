/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.provisionv2;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.powermock.api.mockito.PowerMockito.mock;
import static org.powermock.api.mockito.PowerMockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.junit.Assert;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.reflect.Whitebox;
import org.springframework.util.StringUtils;

import com.google.common.cache.Cache;
import com.vmware.blockchain.deployment.server.BootstrapComponent;
import com.vmware.blockchain.deployment.services.configservice.ConfigServiceInvoker;
import com.vmware.blockchain.deployment.services.configuration.NodeConfiguration;
import com.vmware.blockchain.deployment.services.futureutil.ReactiveStream;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorProvider;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;
import com.vmware.blockchain.deployment.v1.DeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeploymentSpec;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.GenerateConfigurationResponse;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.NodeAssignment;
import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.NodeType;
import com.vmware.blockchain.deployment.v1.OrchestrationSite;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.Properties;
import com.vmware.blockchain.deployment.v1.Sites;
import com.vmware.blockchain.deployment.v1.TransportSecurity;

/**
 * Tests for the ProvisioningServiceV2.generateConfiguration().
 */
@PrepareForTest({NetworkHelper.class, ComputeHelper.class, NodeConfiguration.class, OrchestratorProvider.class,
                 Cache.class, ConfigServiceInvoker.class, ProvisioningServiceV2.class})
public class ProvisioningServiceV2RegenerateConfigurationTest {

    BootstrapComponent bootstrapComponent;
    NetworkHelper networkHelper;
    ComputeHelper computeHelper;
    ConfigHelper configHelper;
    NodeConfiguration nodeConfiguration;
    OrchestratorProvider orchestratorProvider;
    Cache<UUID, CompletableFuture<DeploymentExecutionContext>> deploymentLogCache;

    private DeploymentRequest request = mock(DeploymentRequest.class);
    private DeploymentSpec dSpec = mock(DeploymentSpec.class);
    private MessageHeader msgHeader = mock(MessageHeader.class);
    private Properties props = mock(Properties.class);
    private NodeAssignment nodeAssignment = mock(NodeAssignment.class);
    private Sites sites = mock(Sites.class);
    private List<OrchestrationSite> list = new ArrayList<>();
    private OrchestrationSite orcSite = mock(OrchestrationSite.class);
    private OrchestrationSiteIdentifier orchSiteId = mock(OrchestrationSiteIdentifier.class);
    private OrchestrationSiteInfo orchSideInfo = mock(OrchestrationSiteInfo.class);
    private ConfigurationSessionIdentifier configSessionId = mock(ConfigurationSessionIdentifier.class);

    ProvisioningServiceV2 provisioningServiceV2;

    /**
     * Initialize various mocks.
     */
    @BeforeEach
    public void init() {
        bootstrapComponent = mock(BootstrapComponent.class);
        Endpoint configService = Endpoint.newBuilder().setAddress("test.com")
                .setTransportSecurity(TransportSecurity.newBuilder()
                                              .setType(TransportSecurity.Type.TLSv1_2).build()).build();
        bootstrapComponent.configService = configService;
        bootstrapComponent.pathToCerts = "src/test/resources/certs";

        provisioningServiceV2 = new ProvisioningServiceV2(bootstrapComponent, nodeConfiguration);

        networkHelper = mock(NetworkHelper.class);
        computeHelper = mock(ComputeHelper.class);
        configHelper = mock(ConfigHelper.class);
        nodeConfiguration = mock(NodeConfiguration.class);
        orchestratorProvider = mock(OrchestratorProvider.class);
        deploymentLogCache = mock(Cache.class);

        Whitebox.setInternalState(provisioningServiceV2, NetworkHelper.class, networkHelper);
        Whitebox.setInternalState(provisioningServiceV2, ComputeHelper.class, computeHelper);
        Whitebox.setInternalState(provisioningServiceV2, NodeConfiguration.class, nodeConfiguration);
        Whitebox.setInternalState(provisioningServiceV2, OrchestratorProvider.class, orchestratorProvider);
        Whitebox.setInternalState(provisioningServiceV2, ConfigHelper.class, configHelper);
        Whitebox.setInternalState(provisioningServiceV2, Cache.class, deploymentLogCache);

        when(nodeConfiguration.getDamlSdkVersion()).thenReturn("daml");
        when(nodeConfiguration.getDockerImageBaseVersion()).thenReturn("blockchain-version");

        when(configHelper.generateConfigurationId(any(), any())).thenReturn(configSessionId);
        when(configSessionId.getId()).thenReturn("myCSId");
    }

    /**
     * Clean.
     * */
    @AfterEach
    public void tearDown() {
        provisioningServiceV2 = null;
        deploymentLogCache = null;
    }

    private void basicInitMocks() {
        when(request.getSpec()).thenReturn(dSpec);
        when(request.getHeader()).thenReturn(msgHeader);
        when(msgHeader.getId()).thenReturn("");
        when(dSpec.getConsortiumId()).thenReturn(UUID.randomUUID().toString());
        when(dSpec.getBlockchainId()).thenReturn(UUID.randomUUID().toString());
        when(dSpec.getProperties()).thenReturn(props);
        when(dSpec.getNodeAssignment()).thenReturn(nodeAssignment);
        when(dSpec.getSites()).thenReturn(sites);
        list.add(orcSite);
        when(sites.getInfoListList()).thenReturn(list);
        when(orcSite.getId()).thenReturn(orchSiteId);
        when(orcSite.getInfo()).thenReturn(orchSideInfo);
    }

    /**
     * Very basic smoke test.
     * */
    @Test
    void testGenerateConfigurationDefault() throws Exception {
        basicInitMocks();

        var promise = new CompletableFuture<GenerateConfigurationResponse>();
        provisioningServiceV2.generateConfiguration(request, ReactiveStream.blockedResultObserver(promise));

        Assert.assertNotNull(promise.get().getId());
    }

    private void initMockForDamlDbPassword(Boolean propertyValue, Boolean addTheProperty) {
        Properties.Builder propertiesBuilder = Properties.newBuilder();
        if (addTheProperty) {
            propertiesBuilder.putValues(
                    DeploymentAttributes.GENERATE_DAML_DB_PASSWORD.name(),
                    propertyValue.toString());
        }
        NodeAssignment.Builder nodeAssignment = NodeAssignment.newBuilder();
        nodeAssignment.addEntries(NodeAssignment.Entry
                                          .newBuilder()
                                          .setType(NodeType.REPLICA)
                                          .setNodeId(UUID.randomUUID().toString())
                                          .setSite(
                                                  OrchestrationSiteIdentifier
                                                          .newBuilder()
                                                          .setId(UUID.randomUUID().toString())
                                                          .build())
        );
        UUID clientUuid = UUID.randomUUID();
        nodeAssignment.addEntries(NodeAssignment.Entry
                                          .newBuilder()
                                          .setType(NodeType.CLIENT)
                                          .setNodeId(clientUuid.toString())
                                          .setSite(
                                                  OrchestrationSiteIdentifier
                                                          .newBuilder()
                                                          .setId(UUID.randomUUID().toString())
                                                          .build())
        );

        Map<UUID, List<ConcordComponent>> components = new HashMap<>();
        ConcordComponent cc = ConcordComponent.newBuilder().setServiceType(ConcordComponent.ServiceType.DAML_INDEX_DB)
                .build();
        List<ConcordComponent> comp = Arrays.asList(cc);

        components.put(clientUuid, comp);
        when(nodeConfiguration.generateModelSpec(any(), any(), any())).thenReturn(components);

        Properties props = propertiesBuilder.build();

        when(dSpec.toBuilder()).thenReturn(DeploymentSpec.newBuilder());
        dSpec = dSpec.toBuilder().setProperties(props)
                .setBlockchainId(UUID.randomUUID().toString())
                .setConsortiumId(UUID.randomUUID().toString())
                .setNodeAssignment(nodeAssignment).build();

        request = DeploymentRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder().build())
                .setSpec(dSpec)
                .build();
    }

    /**
     * Verify all is good, without any trace of passwords.
     * */
    @Test
    void testGenerateConfigurationNoPasswordsRequested() throws Exception {
        basicInitMocks();

        var promise = new CompletableFuture<GenerateConfigurationResponse>();
        provisioningServiceV2.generateConfiguration(request, ReactiveStream.blockedResultObserver(promise));
        Assert.assertNotNull(promise.get().getId());

        ArgumentCaptor<DeploymentExecutionContext> captor = ArgumentCaptor.forClass(DeploymentExecutionContext.class);
        ArgumentCaptor<Properties> genericPropsCaptor = ArgumentCaptor.forClass(Properties.class);
        verify(configHelper).generateConfigurationId(captor.capture(), genericPropsCaptor.capture());

        DeploymentExecutionContext dec = captor.getValue();
        List<NodeAssignment.Entry> naEntries = dec.getNodeAssignment().getEntriesList();
        List<NodeAssignment.Entry> list = naEntries.stream().filter(nae -> nae.getProperties()
                .containsValues(NodeProperty.Name.DAML_DB_PASSWORD.name())).collect(Collectors.toList());
        Assert.assertTrue(list.size() == 0);

        Properties props = genericPropsCaptor.getValue();
        Assert.assertFalse(props.getValuesMap().containsKey(DeploymentAttributes.GENERATE_DAML_DB_PASSWORD.name()));
    }

    /**
     * Scenario: no passwords exist, the property enabling them is "True",
     * e.g. new blockchain (1.1 onwards) deployed with DAML Index DB passwords enabled.
     * => passwords should get created.
     * * */
    @Test
    void testGenerateConfigurationWithPasswordHappyPath() throws Exception {
        Boolean enable = true;
        initMockForDamlDbPassword(enable, true);

        var promise = new CompletableFuture<GenerateConfigurationResponse>();
        provisioningServiceV2.generateConfiguration(request, ReactiveStream.blockedResultObserver(promise));
        Assert.assertNotNull(promise.get().getId());

        ArgumentCaptor<DeploymentExecutionContext> captor = ArgumentCaptor.forClass(DeploymentExecutionContext.class);
        ArgumentCaptor<Properties> genericPropsCaptor = ArgumentCaptor.forClass(Properties.class);
        verify(configHelper).generateConfigurationId(captor.capture(), genericPropsCaptor.capture());

        DeploymentExecutionContext dec = captor.getValue();
        List<NodeAssignment.Entry> naEntries = dec.getNodeAssignment().getEntriesList();
        List<NodeAssignment.Entry> list = naEntries.stream().filter(nae -> nae.getProperties()
                .containsValues(NodeProperty.Name.DAML_DB_PASSWORD.name())).collect(Collectors.toList());
        Assert.assertTrue(list.size() == 1);
        NodeAssignment.Entry entry = list.get(0);
        String pwd = entry.getProperties().getValuesMap().get(NodeProperty.Name.DAML_DB_PASSWORD.name());
        Assert.assertTrue(StringUtils.hasText(pwd));

        Properties props = genericPropsCaptor.getValue();
        Assert.assertTrue(props.getValuesMap().containsKey(DeploymentAttributes.GENERATE_DAML_DB_PASSWORD.name()));
        Assert.assertEquals(enable.toString(),
                            props.getValuesMap().get(DeploymentAttributes.GENERATE_DAML_DB_PASSWORD.name()));
    }

    /**
     * Scenario: no passwords exist, the property enabling them is "False",
     * e.g. new blockchain (1.1 onwards) deployed with DAML Index DB passwords disabled.
     * => no passwords should be created.
     * * */
    @Test
    void testGenerateConfigurationNoPasswordWithProp() throws Exception {
        Boolean enable = false;
        initMockForDamlDbPassword(enable, true);

        var promise = new CompletableFuture<GenerateConfigurationResponse>();
        provisioningServiceV2.generateConfiguration(request, ReactiveStream.blockedResultObserver(promise));
        Assert.assertNotNull(promise.get().getId());

        ArgumentCaptor<DeploymentExecutionContext> captor = ArgumentCaptor.forClass(DeploymentExecutionContext.class);
        ArgumentCaptor<Properties> genericPropsCaptor = ArgumentCaptor.forClass(Properties.class);
        verify(configHelper).generateConfigurationId(captor.capture(), genericPropsCaptor.capture());

        DeploymentExecutionContext dec = captor.getValue();
        List<NodeAssignment.Entry> naEntries = dec.getNodeAssignment().getEntriesList();
        List<NodeAssignment.Entry> list = naEntries.stream().filter(nae -> nae.getProperties()
                .containsValues(NodeProperty.Name.DAML_DB_PASSWORD.name())).collect(Collectors.toList());
        Assert.assertTrue(list.size() == 0);

        Properties props = genericPropsCaptor.getValue();
        Assert.assertTrue(props.getValuesMap().containsKey(DeploymentAttributes.GENERATE_DAML_DB_PASSWORD.name()));
        Assert.assertEquals(enable.toString(),
                            props.getValuesMap().get(DeploymentAttributes.GENERATE_DAML_DB_PASSWORD.name()));
    }

    private void initMockExistingPwdsWithProp(String pwd, String propValue) {
        Properties.Builder propertiesBuilder = Properties.newBuilder();
        propertiesBuilder.putValues(
                DeploymentAttributes.GENERATE_DAML_DB_PASSWORD.name(),
                propValue);

        NodeAssignment.Builder nodeAssignment = NodeAssignment.newBuilder();
        nodeAssignment.addEntries(NodeAssignment.Entry
                                          .newBuilder()
                                          .setType(NodeType.REPLICA)
                                          .setNodeId(UUID.randomUUID().toString())
                                          .setSite(
                                                  OrchestrationSiteIdentifier
                                                          .newBuilder()
                                                          .setId(UUID.randomUUID().toString())
                                                          .build())
        );

        Properties.Builder entryPropBuilder = Properties.newBuilder();
        entryPropBuilder.putValues(NodeProperty.Name.DAML_DB_PASSWORD.name(),
                                   pwd);
        UUID clientUuid = UUID.randomUUID();
        nodeAssignment.addEntries(NodeAssignment.Entry
                                          .newBuilder()
                                          .setType(NodeType.CLIENT)
                                          .setNodeId(clientUuid.toString())
                                          .setProperties(entryPropBuilder)
                                          .setSite(
                                                  OrchestrationSiteIdentifier
                                                          .newBuilder()
                                                          .setId(UUID.randomUUID().toString())
                                                          .build())
        );

        Map<UUID, List<ConcordComponent>> components = new HashMap<>();
        ConcordComponent cc = ConcordComponent.newBuilder().setServiceType(ConcordComponent.ServiceType.DAML_INDEX_DB)
                .build();
        List<ConcordComponent> comp = Arrays.asList(cc);

        components.put(clientUuid, comp);
        when(nodeConfiguration.generateModelSpec(any(), any(), any())).thenReturn(components);

        Properties props = propertiesBuilder.build();

        when(dSpec.toBuilder()).thenReturn(DeploymentSpec.newBuilder());
        dSpec = dSpec.toBuilder().setProperties(props)
                .setBlockchainId(UUID.randomUUID().toString())
                .setConsortiumId(UUID.randomUUID().toString())
                .setNodeAssignment(nodeAssignment).build();

        request = DeploymentRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder().build())
                .setSpec(dSpec)
                .build();
    }

    /**
     * Scenario: passwords exist, the property enabling them is "True",
     * e.g. upgrading blockchain (1.1 and later)
     * => no passwords should be changed.
     * * */
    @Test
    void testGenerateConfigurationWithPasswordWithPropTrue() throws Exception {
        String pass = "myPass";
        initMockExistingPwdsWithProp(pass, "true");

        var promise = new CompletableFuture<GenerateConfigurationResponse>();
        provisioningServiceV2.generateConfiguration(request, ReactiveStream.blockedResultObserver(promise));
        Assert.assertNotNull(promise.get().getId());

        ArgumentCaptor<DeploymentExecutionContext> captor = ArgumentCaptor.forClass(DeploymentExecutionContext.class);
        ArgumentCaptor<Properties> genericPropsCaptor = ArgumentCaptor.forClass(Properties.class);
        verify(configHelper).generateConfigurationId(captor.capture(), genericPropsCaptor.capture());

        DeploymentExecutionContext dec = captor.getValue();
        List<NodeAssignment.Entry> naEntries = dec.getNodeAssignment().getEntriesList();
        List<NodeAssignment.Entry> list = naEntries.stream().filter(nae -> nae.getProperties()
                .containsValues(NodeProperty.Name.DAML_DB_PASSWORD.name())).collect(Collectors.toList());
        Assert.assertTrue(list.size() == 1);
        NodeAssignment.Entry entry = list.get(0);
        String pwd = entry.getProperties().getValuesMap().get(NodeProperty.Name.DAML_DB_PASSWORD.name());
        Assert.assertEquals(pass, pwd);

        Properties props = genericPropsCaptor.getValue();
        Assert.assertTrue(props.getValuesMap().containsKey(DeploymentAttributes.GENERATE_DAML_DB_PASSWORD.name()));
        Assert.assertEquals("true", props.getValuesMap().get(DeploymentAttributes.GENERATE_DAML_DB_PASSWORD.name()));
    }

    /**
     * Scenario: passwords exist, the property enabling them is "False",
     * e.g. upgrading blockchain (1.1 and later)
     * => no passwords should be changed.
     * * */
    @Test
    void testGenerateConfigurationWithPasswordWithPropFalse() throws Exception {
        String pass = "myPass";
        initMockExistingPwdsWithProp(pass, "false");

        var promise = new CompletableFuture<GenerateConfigurationResponse>();
        provisioningServiceV2.generateConfiguration(request, ReactiveStream.blockedResultObserver(promise));
        Assert.assertNotNull(promise.get().getId());

        ArgumentCaptor<DeploymentExecutionContext> captor = ArgumentCaptor.forClass(DeploymentExecutionContext.class);
        ArgumentCaptor<Properties> genericPropsCaptor = ArgumentCaptor.forClass(Properties.class);
        verify(configHelper).generateConfigurationId(captor.capture(), genericPropsCaptor.capture());

        DeploymentExecutionContext dec = captor.getValue();
        List<NodeAssignment.Entry> naEntries = dec.getNodeAssignment().getEntriesList();
        List<NodeAssignment.Entry> list = naEntries.stream().filter(nae -> nae.getProperties()
                .containsValues(NodeProperty.Name.DAML_DB_PASSWORD.name())).collect(Collectors.toList());
        Assert.assertTrue(list.size() == 1);
        NodeAssignment.Entry entry = list.get(0);
        String pwd = entry.getProperties().getValuesMap().get(NodeProperty.Name.DAML_DB_PASSWORD.name());
        Assert.assertEquals(pass, pwd);

        Properties props = genericPropsCaptor.getValue();
        Assert.assertTrue(props.getValuesMap().containsKey(DeploymentAttributes.GENERATE_DAML_DB_PASSWORD.name()));
        Assert.assertEquals("false", props.getValuesMap().get(DeploymentAttributes.GENERATE_DAML_DB_PASSWORD.name()));
    }
}
