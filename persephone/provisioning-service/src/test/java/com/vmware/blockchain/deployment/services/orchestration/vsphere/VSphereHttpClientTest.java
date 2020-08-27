/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vsphere;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.delete;
import static com.github.tomakehurst.wiremock.client.WireMock.equalTo;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.patch;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static com.github.tomakehurst.wiremock.client.WireMock.urlPathEqualTo;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.net.URI;
import java.util.AbstractMap;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.runner.RunWith;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.web.util.UriComponentsBuilder;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.VirtualMachinePowerState;
import com.vmware.blockchain.deployment.services.orchestration.vm.CloudInitConfiguration;
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;

@ExtendWith(SpringExtension.class)
@RunWith(PowerMockRunner.class)
@PrepareForTest(VSphereHttpClient.class)
class VSphereHttpClientTest {

    VSphereHttpClient vSphereHttpClient;

    VSphereHttpClient.Context context;

    private static WireMockServer server;

    private static URI vsphereUri;

    private final String goodAuthReturn = "{\n"
                                         + "    \"value\": \"secret string\"\n"
                                         + "}";

    private final String badResponse = "{\n"
            + "    \"value\": [\n"
            + "    ]\n"
            + "}";

    VsphereSessionAuthenticationInterceptor vsphereSessionAuthenticationInterceptor;

    @BeforeEach
    void init() throws Exception {
        server = new WireMockServer(options().dynamicPort());
        server.start();
        // default stub for bad authorization
        server.stubFor(post(urlEqualTo(VsphereEndpoints.VSPHERE_AUTHENTICATION.getPath()))
                               .willReturn(aResponse().withHeader("Content-Type", "application/json")
                                                   .withStatus(401)));

        // stub out the authentication part
        server.stubFor(post(urlEqualTo(VsphereEndpoints.VSPHERE_AUTHENTICATION.getPath()))
                               .withBasicAuth("user", "pass")
                               .willReturn(aResponse().withHeader("Content-Type", "application/json")
                                                   .withBody(goodAuthReturn).withStatus(200)));

        vsphereUri =
                UriComponentsBuilder.newInstance().host("localhost").port(server.port()).scheme("http").build().toUri();
        context = new VSphereHttpClient.Context(vsphereUri, "user", "pass", "");
        vSphereHttpClient = new VSphereHttpClient(context);

    }

    @Test
    void badAuth() throws Exception {
        VSphereHttpClient client =
                new VSphereHttpClient(new VSphereHttpClient.Context(vsphereUri, "no", "body", ""));
    }

    @Test
    void testKeyStoreCreationForSelfSignedCert() {
        // Test Certificate for checking the code path of successfully creating keystore

        // Following is a sample certificate data, just for the test case
        String testCertificateData = "-----BEGIN CERTIFICATE-----\n"
                                     + "MIIDhDCCAmwCCQCqJ2ReGXJGSTANBgkqhkiG9w0BAQsFADCBgzELMAkGA1UEBhMC\n"
                                     + "VVMxCzAJBgNVBAgMAkNBMRIwEAYDVQQHDAlTdW5ueXZhbGUxDzANBgNVBAoMBlZN\n"
                                     + "d2FyZTENMAsGA1UECwwET0NUTzETMBEGA1UEAwwKdm13YXJlLmNvbTEeMBwGCSqG\n"
                                     + "SIb3DQEJARYPdGVzdEB2bXdhcmUuY29tMB4XDTIwMDgyNDIyMDI0M1oXDTIxMDgy\n"
                                     + "NDIyMDI0M1owgYMxCzAJBgNVBAYTAlVTMQswCQYDVQQIDAJDQTESMBAGA1UEBwwJ\n"
                                     + "U3Vubnl2YWxlMQ8wDQYDVQQKDAZWTXdhcmUxDTALBgNVBAsMBE9DVE8xEzARBgNV\n"
                                     + "BAMMCnZtd2FyZS5jb20xHjAcBgkqhkiG9w0BCQEWD3Rlc3RAdm13YXJlLmNvbTCC\n"
                                     + "ASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAKQ53M+a9Rl2n6uR6+Gl2ErT\n"
                                     + "MMwXRixfcdQkBaSfnENf4rSLCo/9nbUFzSDI2N7USY1FlmzHeyvopRNmuyCda6Jv\n"
                                     + "oleleiaHhyrR0FMAUZ8Vz0sI4fMRaRqKBsMJ+QgX4USdmghAkmys7ig5MUJjcU8D\n"
                                     + "UoQ4LoUDiGARkw0oD6cNWa3pdWVfJ3mvaHuq1OlZfQ3kC1luyklhihPIMGrisuua\n"
                                     + "49tGBZs3F6n3Ky3hU09I3okKkBtioXTkYz3Bdszt/XMS5HeyrX/nG2NO60RT3OVX\n"
                                     + "nvdb0bmPSJNmnvSGEzTD8WnA+9Vg13e8xLLA1W6+oFhG6rUD5g5IKnfl6zE8sL0C\n"
                                     + "AwEAATANBgkqhkiG9w0BAQsFAAOCAQEANb0u4elmBugWqAR9reQRlk66Nx3Velab\n"
                                     + "NSI8f78WGHMCS4ryG8fwFKwcJ9XlDJZA81FcRDrycvk0qaLgnSWnBrrauDDkGRL4\n"
                                     + "mJsFnjBIfJkJXDvfp6LYhgubleVj5kiNHCp/Pp9wFnuP7/Q8fgygZrXSiT/tzAQy\n"
                                     + "Au90vbEzxvpCvbf0lQYcL5M9jFt9D2RxMLWQCbf+PrluXe+leWOiPlrvXY/sYqxW\n"
                                     + "hlkQfaYjauR4qTbqo9VX4q142tbAsGnq8tmXbAlL+NK12HHZvGjOlsyOUjAbD0Vt\n"
                                     + "398Vtqg3kiL/IAe1weda08BdTkA/Dj4DKHZQHs2ndKoDVV5icoIXHw==\n"
                                     + "-----END CERTIFICATE-----";
        String testCertificateUrl = "vmware.com";

        context = new VSphereHttpClient.Context(vsphereUri, "user", "pass", testCertificateData);
        VSphereHttpClient client = new VSphereHttpClient(context);
    }

    @Test
    void testGetGoodFolder() {
        String goodFolderResponseString = "{\n"
                                          + "    \"value\": [\n"
                                          + "        {\n"
                                          + "            \"folder\": \"Offensive Spear Gungnir\",\n"
                                          + "            \"name\": \"Good_folder\",\n"
                                          + "            \"type\": \"VIRTUAL_MACHINE\"\n"
                                          + "        }"
                                          + "    ]\n"
                                          + "}";

        String goodFolder = "Offensive Spear Gungnir";
        String goodFolderName = "Good_folder";

        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_FOLDERS.getPath()))
                               .withQueryParam("filter.type", equalTo("VIRTUAL_MACHINE"))
                               .withQueryParam("filter.names", equalTo(goodFolderName))
                               .willReturn(aResponse()
                                                   .withHeader("Content-Type", "application/json")
                                                   .withBody(goodFolderResponseString).withStatus(200)));

        String folder = vSphereHttpClient.getFolder(goodFolderName);
        Assertions.assertEquals(goodFolder, folder);
    }

    @Test
    void testGetBadFolder() {
        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_FOLDERS.getPath()))
                .willReturn(aResponse().withHeader("Content-Type", "application/json")
                        .withBody(badResponse).withStatus(200)));

        String badFolderName = "Bad_folder";
        Assertions.assertThrows(
                PersephoneException.class,
            () -> vSphereHttpClient.getFolder(badFolderName)
        );
    }

    @Test
    void getGoodResourcePool() {
        String goodResourcePoolResponseString = "{\n"
                + "    \"value\": [\n"
                + "        {\n"
                + "            \"resource_pool\": \"Tiny Adventurer\",\n"
                + "            \"name\": \"Good_resource_pool\"\n"
                + "        }"
                + "    ]\n"
                + "}";

        String goodResourcePool = "Tiny Adventurer";
        String goodResourcePoolName = "Good_resource_pool";

        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_RESOURCE_POOLS.getPath()))
                .withQueryParam("filter.names", equalTo(goodResourcePoolName))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(goodResourcePoolResponseString).withStatus(200)));



        String resourcePool = vSphereHttpClient.getResourcePool(goodResourcePoolName);

        Assertions.assertEquals(goodResourcePool, resourcePool);
    }

    @Test
    void testGetBadResourcePool() {
        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_RESOURCE_POOLS.getPath()))
                .willReturn(aResponse().withHeader("Content-Type", "application/json")
                        .withBody(badResponse).withStatus(200)));

        String badResourcePoolName = "Bad_resource_pool";
        Assertions.assertThrows(
                PersephoneException.class,
            () -> vSphereHttpClient.getResourcePool(badResourcePoolName)
        );
    }

    @Test
    void getGoodDatastore() {
        String goodDatastoreResponseString = "{\n"
                + "    \"value\": [\n"
                + "        {\n"
                + "            \"datastore\": \"Searching for bliss\",\n"
                + "            \"name\": \"Good_datastore\",\n"
                + "            \"type\": \"Good_datastore_type\",\n"
                + "            \"free_space\": 100,\n"
                + "            \"capacity\": 200\n"
                + "        }"
                + "    ]\n"
                + "}";

        String goodDatastore = "Searching for bliss";
        String goodDatastoreName = "Good_datastore";

        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_DATASTORES.getPath()))
                .withQueryParam("filter.names", equalTo(goodDatastoreName))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(goodDatastoreResponseString).withStatus(200)));

        String datastore = vSphereHttpClient.getDatastore(goodDatastoreName);

        Assertions.assertEquals(goodDatastore, datastore);
    }

    @Test
    void getBadDatastore() {
        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_DATASTORES.getPath()))
                .willReturn(aResponse().withHeader("Content-Type", "application/json")
                        .withBody(badResponse).withStatus(200)));

        String badDatastoreName = "Bad_datastore";
        Assertions.assertThrows(
                PersephoneException.class,
            () -> vSphereHttpClient.getDatastore(badDatastoreName)
        );
    }

    @Test
    void getGoodNetwork() {
        String goodNetworkResponseString = "{\n"
                + "    \"value\": [\n"
                + "        {\n"
                + "            \"network\": \"BLOODY STREAM\",\n"
                + "            \"name\": \"Good_network\",\n"
                + "            \"type\": \"Good_network_type\"\n"
                + "        }"
                + "    ]\n"
                + "}";

        String goodNetwork = "BLOODY STREAM";
        String goodNetworkName = "Good_network";

        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_NETWORKS.getPath()))
                .withQueryParam("filter.names", equalTo(goodNetworkName))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(goodNetworkResponseString).withStatus(200)));



        String network = vSphereHttpClient.getNetwork(goodNetworkName);

        Assertions.assertEquals(goodNetwork, network);
    }

    @Test
    void getBadNetwork() {
        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_NETWORKS.getPath()))
                .willReturn(aResponse().withHeader("Content-Type", "application/json")
                        .withBody(badResponse).withStatus(200)));

        String badNetworkName = "Bad_network";
        Assertions.assertThrows(
                PersephoneException.class,
            () -> vSphereHttpClient.getNetwork(badNetworkName)
        );
    }

    @Test
    void getGoodLibraryItem() {
        String goodLibraryItemResponseString = "{\n"
                + "    \"value\": [\"Monochrome Classroom\"]\n"
                + "}";

        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_CONTENT_LIBRARY_ITEM.getPath()))
                .withQueryParam("~action", WireMock.equalTo("find"))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(goodLibraryItemResponseString).withStatus(200)));

        String sourceId = "sourceId";
        String libraryItem = vSphereHttpClient.getLibraryItem(sourceId);

        String goodLibraryItem = "Monochrome Classroom";
        Assertions.assertEquals(goodLibraryItem, libraryItem);
    }

    @Test
    void getBadLibraryItem() {
        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_CONTENT_LIBRARY_ITEM.getPath()))
                .withQueryParam("~action", WireMock.equalTo("find"))
                .willReturn(aResponse().withHeader("Content-Type", "application/json")
                        .withBody(badResponse).withStatus(200)));

        String sourceId = "sourceId";
        Assertions.assertThrows(
                PersephoneException.class,
            () -> vSphereHttpClient.getLibraryItem(sourceId)
        );
    }

    @Test
    void createVirtualMachine() {
        String goodVmCreateResponseString = "{\n"
                + "    \"value\": \n"
                + "        {\n"
                + "            \"succeeded\": true,\n"
                + "            \"resource_id\": {\n"
                + "                 \"type\": \"Fire\",\n"
                + "                 \"id\": \"Steins:Gate\""
                + "            }"
                + "        }"
                + "}";

        String libraryItem = "libraryItem";
        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_OVF_LIBRARY_ITEM.getPath().replace("{library_item}",
                libraryItem)))
                .withQueryParam("~action", WireMock.equalTo("deploy"))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(goodVmCreateResponseString).withStatus(200)));

        String name = "Inverted Bridge";
        String datastore = "dataStore";
        String resourcePool = "resourcePool";
        String folder = "folder";
        Map.Entry<String, String> networks = new AbstractMap.SimpleEntry<>("frip", "Side");

        CloudInitConfiguration cloudInit = mock(CloudInitConfiguration.class);
        when(cloudInit.userData()).thenReturn("Gibberish for testing purposes");

        String createVirtualMachine = vSphereHttpClient.createVirtualMachine(name, libraryItem, datastore,
                resourcePool, folder, networks, cloudInit);

        Assertions.assertEquals(createVirtualMachine, "Steins:Gate");
    }

    @Test
    void createVirtualMachineWithRetry() {
        String goodVmCreateResponseString = "{\n"
                                            + "    \"value\": \n"
                                            + "        {\n"
                                            + "            \"succeeded\": true,\n"
                                            + "            \"resource_id\": {\n"
                                            + "                 \"type\": \"Fire\",\n"
                                            + "                 \"id\": \"Steins:Gate\""
                                            + "            }"
                                            + "        }"
                                            + "}";

        String emtpyVmCreateResponseString = "{\n"
                                            + "    \"value\": \n"
                                            + "        {\n"
                                            + "            \"succeeded\": true\n"
                                            + "        }"
                                            + "}";

        String libraryItem = "libraryItem";
        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_OVF_LIBRARY_ITEM.getPath().replace("{library_item}",
                                                                                                       libraryItem)))
                               .withQueryParam("~action", WireMock.equalTo("deploy"))
                               .willReturn(aResponse()
                                                   .withHeader("Content-Type", "application/json")
                                                   .withBody(emtpyVmCreateResponseString).withStatus(200)));

        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_OVF_LIBRARY_ITEM.getPath().replace("{library_item}",
                                                                                                       libraryItem)))
                               .withQueryParam("~action", WireMock.equalTo("deploy"))
                               .willReturn(aResponse()
                                                   .withHeader("Content-Type", "application/json")
                                                   .withBody(goodVmCreateResponseString).withStatus(200)));

        String name = "Inverted Bridge";
        String datastore = "dataStore";
        String resourcePool = "resourcePool";
        String folder = "folder";
        Map.Entry<String, String> networks = new AbstractMap.SimpleEntry<>("frip", "Side");

        CloudInitConfiguration cloudInit = mock(CloudInitConfiguration.class);
        when(cloudInit.userData()).thenReturn("Gibberish for testing purposes");

        String createVirtualMachine = vSphereHttpClient.createVirtualMachine(name, libraryItem, datastore,
                                                                             resourcePool, folder, networks, cloudInit);

        Assertions.assertEquals(createVirtualMachine, "Steins:Gate");
    }

    @Test
    void createVirtualMachineWithRetryFailure() {

        String emtpyVmCreateResponseString = "{\n"
                                             + "    \"value\": \n"
                                             + "        {\n"
                                             + "            \"succeeded\": true\n"
                                             + "        }"
                                             + "}";

        String libraryItem = "libraryItem";
        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_OVF_LIBRARY_ITEM.getPath().replace("{library_item}",
                                                                                                       libraryItem)))
                               .withQueryParam("~action", WireMock.equalTo("deploy"))
                               .willReturn(aResponse()
                                                   .withHeader("Content-Type", "application/json")
                                                   .withBody(emtpyVmCreateResponseString).withStatus(200)));

        String name = "Inverted Bridge";
        String datastore = "dataStore";
        String resourcePool = "resourcePool";
        String folder = "folder";
        Map.Entry<String, String> networks = new AbstractMap.SimpleEntry<>("frip", "Side");

        CloudInitConfiguration cloudInit = mock(CloudInitConfiguration.class);
        when(cloudInit.userData()).thenReturn("Gibberish for testing purposes");

        Assertions.assertThrows(
                PersephoneException.class,
            () -> vSphereHttpClient
                        .createVirtualMachine(name, libraryItem, datastore, resourcePool, folder, networks, cloudInit)
        );
    }

    @Test
    void updateVirtualMachineMemory() {
        String name = "skyv2008";

        server.stubFor(patch(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_MEMORY_UPDATE.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        long memory = 1000;
        Assertions.assertTrue(vSphereHttpClient.updateVirtualMachineMemory(name, memory));
    }

    @Test
    void updateVirtualMachineCpu() {
        String name = "Jumpin";
        server.stubFor(patch(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_CPU_UPDATE.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        Assertions.assertTrue(vSphereHttpClient.updateVirtualMachineCpu(name, 10, 10));
    }

    @Test
    void createVirtualMachineDisk() {
        String name = "vm-007";

        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_DISK_CREATE.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        long diskGb = 64;
        Assertions.assertTrue(vSphereHttpClient.createVirtualMachineDisk(name, diskGb));
    }

    @Test
    void createVirtualMachineDiskBad() {
        String name = "vm-008";

        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_DISK_CREATE.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(400)));

        long diskGb = -1;
        Assertions.assertFalse(vSphereHttpClient.createVirtualMachineDisk(name, diskGb));
    }

    @Test
    void deleteExistingVirtualMachine() {
        String id = "Snowstorm";
        server.stubFor(delete(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM.getPath().replace("{vm}", id)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        Assertions.assertTrue(vSphereHttpClient.deleteVirtualMachine(id));
    }

    @Test
    void deleteNotFoundVirtualMachine() {
        String id = "Fleeting";

        server.stubFor(delete(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM.getPath().replace("{vm}", id)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(404)));

        Assertions.assertTrue(vSphereHttpClient.deleteVirtualMachine(id));
    }

    @Test
    void deleteForbiddenVirtualMachine() {
        String id = "Backlit_Wings";

        server.stubFor(delete(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM.getPath().replace("{vm}", id)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(403)));

        Assertions.assertFalse(vSphereHttpClient.deleteVirtualMachine(id));
    }

    @Test
    void powerOnVirtualMachine() {
        String name = "Snowy_Daydream";
        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_POWER_START.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        Assertions.assertTrue(vSphereHttpClient.updateVirtualMachinePowerState(name,
                VirtualMachinePowerState.POWERED_ON));
    }

    @Test
    void powerOffVirtualMachine() {
        String name = "FIXED_STAR";
        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_POWER_STOP.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        Assertions.assertTrue(vSphereHttpClient.updateVirtualMachinePowerState(name,
                VirtualMachinePowerState.POWERED_OFF));
    }

    @Test
    void getVirtualMachinePower() {
        String goodVmPowerResponseString = "{\n"
                + "    \"value\": \n"
                + "        {\n"
                + "            \"state\": \"POWERED_ON\"\n"
                + "        }"
                + "}";

        String name = "Marionette";
        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_POWER.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(goodVmPowerResponseString).withStatus(200)));

        Assertions.assertEquals(VirtualMachinePowerState.POWERED_ON, vSphereHttpClient.getVirtualMachinePower(name));
    }

    @Test
    void getVirtualMachinePowerBadResponse() {
        String name = "More_One_Night";

        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_POWER.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(404)));

        Assertions.assertThrows(
                PersephoneException.class,
            () -> vSphereHttpClient.getVirtualMachinePower(name)
        );
    }

    @Test
    void ensureVirtualMachinePowerStart() {
        String name = "promenade";

        String goodVmPowerResponseString = "{\n"
                + "    \"value\": \n"
                + "        {\n"
                + "            \"state\": \"POWERED_ON\"\n"
                + "        }"
                + "}";

        // Turn on VM Power
        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_POWER_START.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        // Get power status
        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_POWER.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(goodVmPowerResponseString).withStatus(200)));

        // Update VM memory
        server.stubFor(patch(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_MEMORY_UPDATE.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        // Update VM CPU
        server.stubFor(patch(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_CPU_UPDATE.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        // Create VM Disk
        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_DISK_CREATE.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        Map<String, String> properties = new HashMap<String, String>();

        properties.put(DeploymentAttributes.VM_MEMORY.toString(), "16");
        properties.put(DeploymentAttributes.VM_CPU_COUNT.toString(), "32");
        properties.put(DeploymentAttributes.VM_CORES_PER_SOCKET.toString(), "64");
        properties.put(DeploymentAttributes.VM_STORAGE.toString(), "67");

        long retryInterval = 10;
        Assertions.assertTrue(vSphereHttpClient.ensureVirtualMachinePowerStart(name, retryInterval,
                properties));
    }

    @Test
    void ensureVirtualMachinePowerStop() {
        String name = "Love_Marginal";

        String goodVmPowerResponseString = "{\n"
                + "    \"value\": \n"
                + "        {\n"
                + "            \"state\": \"POWERED_OFF\"\n"
                + "        }"
                + "}";

        // Turn off VM Power
        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_POWER_STOP.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        // Get power status
        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_POWER.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(goodVmPowerResponseString).withStatus(200)));

        Assertions.assertTrue(vSphereHttpClient.ensureVirtualMachinePowerStop(name));
    }
}