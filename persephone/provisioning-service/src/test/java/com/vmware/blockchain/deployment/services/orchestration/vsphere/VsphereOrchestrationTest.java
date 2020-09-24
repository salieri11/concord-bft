/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vsphere;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.delete;
import static com.github.tomakehurst.wiremock.client.WireMock.equalTo;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static com.github.tomakehurst.wiremock.client.WireMock.urlPathEqualTo;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.notNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.doReturn;
import static org.powermock.api.mockito.PowerMockito.spy;

import java.net.URI;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.util.UriComponentsBuilder;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.google.gson.FieldNamingPolicy;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorData;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorSiteInformation;
import com.vmware.blockchain.deployment.services.orchestration.ipam.IpamClient;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.DatastoreSummary;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.FolderSummary;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.GetDatastoreResponse;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.GetFolderResponse;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.GetNetworkResponse;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.GetResourcePoolResponse;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.LibraryItemFindResponse;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.NetworkSummary;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.ResourcePoolSummary;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.IPv4Network;
import com.vmware.blockchain.deployment.v1.VSphereDatacenterInfo;
import com.vmware.blockchain.deployment.v1.VSphereOrchestrationSiteInfo;

@RunWith(PowerMockRunner.class)
class VsphereOrchestrationTest {

    VSphereHttpClient vSphereHttpClient;
    VSphereOrchestrationSiteInfo vSphereOrchestrationSiteInfo;
    VSphereDatacenterInfo vSphereDatacenterInfo;
    IpamClient ipamClient;
    VSphereOrchestrator vSphereOrchestrator;
    OrchestratorSiteInformation orchestratorSiteInformation;

    WireMockServer server;
    Gson gson = new GsonBuilder()
            .setFieldNamingPolicy(FieldNamingPolicy.LOWER_CASE_WITH_UNDERSCORES)
            .create();

    private static final String LOGGED_IN_SESSION = "LOGGED_IN_SESSION";
    private static final String LOGGED_OUT_SESSION = "LOGGED_OUT_SESSION";
    private static final String DEPLOYSCENARIO = "DEPLOYSCENARIO";

    private static final String RESOURCEPOOL = "RESOURCEPOOL";

    @BeforeEach
    void init() {
        server = new WireMockServer(options().dynamicPort());
        stubSessionLogin();
        stubSessionLogout();
        stubGetFolder();
        stubGetDatastore();
        stubGetResourcePool();
        stubGetNetwork();
        stubGetLibraryItem();

        server.start();
        URI vsphereUri = UriComponentsBuilder.newInstance().host("localhost")
                .port(server.port()).scheme("http").build().toUri();
        VSphereHttpClient.Context context = new VSphereHttpClient.Context(vsphereUri, "user", "pass", "");
        vSphereHttpClient = new VSphereHttpClient(context);
        vSphereHttpClient = spy(vSphereHttpClient);

        // Stub client operations as they are too painful to stub on the server
        stubCreateVirtualMachine();
        stubEnsureVirtualMachinePowerStart();

        vSphereOrchestrationSiteInfo = mock(VSphereOrchestrationSiteInfo.class);
        when(vSphereOrchestrationSiteInfo.getVsphere()).thenReturn(vSphereDatacenterInfo);

        vSphereDatacenterInfo = mock(VSphereDatacenterInfo.class);
        when(vSphereDatacenterInfo.getResourcePool()).thenReturn(RESOURCEPOOL);
        when(vSphereDatacenterInfo.getDatastore()).thenReturn("datastore");

        OrchestratorSiteInformation orchestratorSiteInformation = mock(OrchestratorSiteInformation.class);
        when(orchestratorSiteInformation.getDataStore()).thenReturn("datastore");
        when(orchestratorSiteInformation.getResourcePool()).thenReturn("resourcepool");
        when(orchestratorSiteInformation.getFolder()).thenReturn("folder");
        when(orchestratorSiteInformation.getNetwork()).thenReturn("network");

        IPv4Network network = IPv4Network.newBuilder().build();
        when(vSphereDatacenterInfo.getNetwork()).thenReturn(network);
        vSphereOrchestrator = new VSphereOrchestrator(
            vSphereDatacenterInfo,
            Endpoint.newBuilder().build(),
            ipamClient);

        // Inject these into the orchestrator
        ReflectionTestUtils.setField(vSphereOrchestrator, "vSphereHttpClient", vSphereHttpClient);
        ReflectionTestUtils.setField(vSphereOrchestrator, "orchestratorSiteInformation", orchestratorSiteInformation);
    }

    @AfterEach
    void done() {
        server.stop();
    }

    private void stubSessionLogin() {
        JsonObject o = new JsonObject();
        o.addProperty("value", "12345");
        String response = gson.toJson(o);
        server.stubFor(
                post(urlEqualTo(VsphereEndpoints.VSPHERE_AUTHENTICATION.getPath()))
                .inScenario(DEPLOYSCENARIO)
                        .willSetStateTo(LOGGED_IN_SESSION)
                .willReturn(aResponse().withHeader("Content-Type", "application/json")
                        .withBody(response)
                        .withStatus(200))

        );
    }

    private void stubSessionLogout() {
        server.stubFor(
                delete(urlEqualTo(VsphereEndpoints.VSPHERE_AUTHENTICATION.getPath()))
                .inScenario(DEPLOYSCENARIO)
                        .whenScenarioStateIs(LOGGED_IN_SESSION)
                        .willSetStateTo(LOGGED_OUT_SESSION)
                .willReturn(aResponse().withStatus(200)));
    }

    private void stubGetFolder() {
        FolderSummary summary = new FolderSummary();
        summary.setFolder("folder");
        summary.setName("folder");
        summary.setType("VIRTUAL_MACHINE");
        GetFolderResponse response = new GetFolderResponse();
        response.setValue(List.of(summary));

        String folderResponseString = gson.toJson(response, GetFolderResponse.class);

        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_FOLDERS.getPath()))
                .inScenario(DEPLOYSCENARIO)
                        .whenScenarioStateIs(LOGGED_IN_SESSION)
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(folderResponseString).withStatus(200)));
    }

    private void stubGetDatastore() {

        DatastoreSummary summary = new DatastoreSummary();
        summary.setDatastore("datastore");
        summary.setName("datastore");
        summary.setType("type");
        summary.setCapacity(10L);
        summary.setFreeSpace(100L);
        GetDatastoreResponse datastoreResponse = new GetDatastoreResponse();
        datastoreResponse.setValue(List.of(summary));
        String datastoreResponseString = gson.toJson(datastoreResponse, GetDatastoreResponse.class);

        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_DATASTORES.getPath()))
                .inScenario(DEPLOYSCENARIO)
                    .whenScenarioStateIs(LOGGED_IN_SESSION)
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(datastoreResponseString).withStatus(200)));

        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_DATASTORES.getPath()))
                .inScenario(DEPLOYSCENARIO)
                .whenScenarioStateIs(LOGGED_OUT_SESSION)
                .willReturn(aResponse().withStatus(401)));
    }

    private void stubGetResourcePool() {
        GetResourcePoolResponse resourcePoolResponse = new GetResourcePoolResponse();
        ResourcePoolSummary summary = new ResourcePoolSummary();
        summary.setResourcePool(RESOURCEPOOL);
        summary.setName(RESOURCEPOOL);
        resourcePoolResponse.setValue(List.of(summary));

        String resourcePoolResponseString = gson.toJson(resourcePoolResponse, GetResourcePoolResponse.class);

        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_RESOURCE_POOLS.getPath()))
                .withQueryParam("filter.names", equalTo(RESOURCEPOOL))
                .inScenario(DEPLOYSCENARIO)
                    .whenScenarioStateIs(LOGGED_IN_SESSION)
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(resourcePoolResponseString).withStatus(200)));
    }

    private void stubGetNetwork() {
        NetworkSummary summary = new NetworkSummary();
        summary.setName("network");
        summary.setNetwork("network");
        summary.setType("type");
        GetNetworkResponse networkResponse = new GetNetworkResponse();
        networkResponse.setValue(List.of(summary));

        String networkResponseString = gson.toJson(networkResponse, GetNetworkResponse.class);

        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_NETWORKS.getPath()))
                .inScenario(DEPLOYSCENARIO)
                    .whenScenarioStateIs(LOGGED_IN_SESSION)
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(networkResponseString).withStatus(200)));
    }

    private void stubGetLibraryItem() {
        LibraryItemFindResponse libraryItemFindResponse = new LibraryItemFindResponse();
        libraryItemFindResponse.setValue(List.of("libraryitem"));
        String libraryItemFindResponseString = gson.toJson(libraryItemFindResponse, LibraryItemFindResponse.class);
        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_CONTENT_LIBRARY_ITEM.getPath()))
                .inScenario(DEPLOYSCENARIO)
                    .whenScenarioStateIs(LOGGED_IN_SESSION)
                .withQueryParam("~action", WireMock.equalTo("find"))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(libraryItemFindResponseString).withStatus(200)));
    }

    private void stubCreateVirtualMachine() {
        doReturn("virtualmachine").when(vSphereHttpClient).createVirtualMachine(
                anyString(), anyString(), anyString(), anyString(), anyString(),
                notNull(), notNull()
        );
    }

    private void stubEnsureVirtualMachinePowerStart() {
        doReturn(true).when(vSphereHttpClient).ensureVirtualMachinePowerStart(anyString(), anyLong(), anyMap());
    }

    private void doLogout() {
        String uri = VsphereEndpoints.VSPHERE_AUTHENTICATION.getPath();
        HttpEntity requests = new HttpEntity(new HttpHeaders());
        ResponseEntity<Void> response =
                vSphereHttpClient.restTemplate().exchange(uri, HttpMethod.DELETE, requests, Void.class);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testVSphereDeployment() {

        OrchestratorData.CreateComputeResourceRequestV2.CloudInitData cloudInitData =
                new OrchestratorData.CreateComputeResourceRequestV2.CloudInitData(
                        Endpoint.newBuilder().build(),
                        Endpoint.newBuilder().build(),
                        ConcordModelSpecification.newBuilder().build(),
                        "privateIp",
                        ConfigurationSessionIdentifier.newBuilder().build(),
                        Endpoint.newBuilder().build(),
                        Endpoint.newBuilder().build()
                );
        OrchestratorData.CreateComputeResourceRequestV2 request =
                new OrchestratorData.CreateComputeResourceRequestV2(
                        UUID.randomUUID(),
                        UUID.randomUUID(),
                        cloudInitData,
                        Collections.emptyMap()
                );

        // This should pass
        String dataStore = vSphereHttpClient.getDatastore("datastore");
        assertEquals("datastore", dataStore);

        // This should also pass
        vSphereOrchestrator.createDeploymentV2(request);

        // Invalidate session, and retry
        doLogout();

        assertDoesNotThrow(() -> {
            String myDataStore = vSphereHttpClient.getDatastore("datastore");
            // This should pass since refreshCredentials() will be called due to logout above
            assertEquals("datastore", myDataStore);
        });
    }
}
