/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.clients;

import static com.vmware.blockchain.security.MvcTestSecurityConfig.createContext;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.authentication;
import static org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers.springSecurity;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.MediaType;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableList;
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.common.HelenExceptionHandler;
import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.security.MvcTestSecurityConfig;
import com.vmware.blockchain.security.SecurityTestUtils;
import com.vmware.blockchain.services.blockchains.Blockchain;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.blockchains.BlockchainUtils;
import com.vmware.blockchain.services.blockchains.replicas.Replica;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;
import com.vmware.blockchain.services.blockchains.zones.VmcAwsZone;
import com.vmware.blockchain.services.blockchains.zones.Zone;
import com.vmware.blockchain.services.blockchains.zones.ZoneService;
import com.vmware.blockchain.services.blockchains.zones.ZoneTestUtils;
import com.vmware.blockchain.services.models.NodeGetCredentialsResponse;
import com.vmware.blockchain.services.profiles.Consortium;
import com.vmware.blockchain.services.profiles.ConsortiumService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.Organization;
import com.vmware.blockchain.services.profiles.OrganizationService;
import com.vmware.blockchain.services.profiles.VmbcRoles;
import com.vmware.blockchain.services.tasks.Task;
import com.vmware.blockchain.services.tasks.TaskController;
import com.vmware.blockchain.services.tasks.TaskService;


/**
 * Tests for the client controller.
 */
@ExtendWith(SpringExtension.class)
@WebMvcTest(controllers = { ClientController.class, TaskController.class })
@ContextConfiguration(classes = {MvcTestSecurityConfig.class, MvcConfig.class})
@ComponentScan(basePackageClasses = { ClientControllerTest.class, HelenExceptionHandler.class,
                                      TaskController.class})
public class ClientControllerTest extends RuntimeException {

    static final UUID BC_DAML = UUID.fromString("fd7167b0-057d-11ea-8d71-362b9e155667");
    static final UUID C2_ID = UUID.fromString("04e4f62d-5364-4363-a582-b397075b65a3");
    static final UUID C3_ID = UUID.fromString("a4b8f7ed-00b3-451e-97bc-4aa51a211288");
    static final UUID TASK_ID = UUID.fromString("c23ed97d-f29c-472e-9f63-cc6be883a5f5");
    static final UUID ORG_ID = UUID.fromString("5c373085-0cd1-47e4-b4f2-66d418f22fdf");
    static final UUID ORG2_ID = UUID.fromString("a774d0e3-b182-4330-93df-6738c8b1b2de");
    static final UUID DEP_ID = UUID.fromString("67376aed-333c-4e35-b6b6-c59800752dc3");
    private static final UUID SITE_1 = UUID.fromString("84b9a0ed-c162-446a-b8c0-2e45755f3844");
    private static final UUID SITE_2 = UUID.fromString("275638a3-8860-4925-85de-c73d45cb7232");
    private static final UUID REPLICA_1 = UUID.fromString("73ba5deb-1046-48e3-a369-3982177cabed");
    private static final UUID REPLICA_2 = UUID.fromString("d3776a28-7d37-4a75-b1d8-7832eec6badb");
    private static final UUID REPLICA_3 = UUID.fromString("5a2e1527-f31d-4099-8c09-a320b671046b");
    private static final UUID REPLICA_4 = UUID.fromString("3ca83dfe-62a1-4226-b31a-706a60f4dc28");
    private static final UUID REPLICA_1_ZONE = UUID.fromString("987ec776-679f-4428-9135-4db872a0a64b");
    private static final UUID REPLICA_2_ZONE = UUID.fromString("2462e0bf-ccbd-4e7b-b5ee-70514d3188cf");
    private static final UUID REPLICA_3_ZONE = UUID.fromString("e10a68e3-faa5-4ab6-a69f-53e10bc0d490");
    private static final UUID REPLICA_4_ZONE = UUID.fromString("493b84d7-5333-4294-bba7-c56ade48cb73");

    @Autowired
    private WebApplicationContext context;

    private MockMvc mockMvc;

    @MockBean
    DefaultProfiles defaultProfiles;

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;

    @MockBean
    ConsortiumService consortiumService;

    @MockBean
    BlockchainService blockchainService;

    @MockBean
    ReplicaService replicaService;

    @MockBean
    ClientService clientService;

    @MockBean
    OperationContext operationContext;

    @MockBean
    OrganizationService organizationService;

    @MockBean
    ZoneService zoneService;

    @Autowired
    TaskService taskService;

    @Autowired
    AuthHelper authHelper;

    private ObjectMapper objectMapper;

    private AuthenticationContext adminAuth;
    private AuthenticationContext consortiumAuth;
    private AuthenticationContext userAuth;
    private AuthenticationContext user2Auth;

    /**
     * Initialize various mocks.
     */
    @BeforeEach
    public void init() {
        mockMvc = MockMvcBuilders
                .webAppContextSetup(context)
                .apply(springSecurity())
                .build();
        Consortium consortium = SecurityTestUtils.getConsortium();

        when(operationContext.getId()).thenReturn(UUID.randomUUID().toString());

        UUID c1Id = consortium.getId();
        when(consortiumService.get(c1Id)).thenReturn(consortium);


        final Blockchain bcdaml = Blockchain.builder()
                .consortium(UUID.fromString("5a0cebc0-057e-11ea-8d71-362b9e155667"))
                .nodeList(Stream.of("one", "two", "three")
                                  .map(s -> new Blockchain.NodeEntry(UUID.randomUUID(), s, s, "http://".concat(s),
                                                                     "cert-".concat(s), SITE_2))
                                  .collect(Collectors.toList()))
                .type(Blockchain.BlockchainType.DAML)
                .build();

        final Replica replica1 = new Replica("publicIp", "privateIp", "hostName", "url", "cert", REPLICA_1_ZONE,
                                             Replica.ReplicaType.NONE, BC_DAML, "testPassword");
        replica1.setId(REPLICA_1);

        final Replica replica2 = new Replica("publicIp", "privateIp", "hostName", "url", "cert", REPLICA_2_ZONE,
                                             Replica.ReplicaType.DAML_PARTICIPANT, BC_DAML, "testPassword");
        replica2.setId(REPLICA_2);

        final Replica replica3 = new Replica("publicIp", "privateIp", "hostName", "url", "cert", REPLICA_3_ZONE,
                                             Replica.ReplicaType.DAML_PARTICIPANT, BC_DAML, "testPassword");
        replica3.setId(REPLICA_3);

        final Replica replica4 = new Replica("publicIp", "privateIp", "hostName", "url", "cert", REPLICA_4_ZONE,
                                             Replica.ReplicaType.NONE, BC_DAML, "testPassword");
        replica4.setId(REPLICA_4);

        when(replicaService.getReplicas(BC_DAML))
                .thenReturn(ImmutableList.of(replica1, replica2, replica3, replica4));
        bcdaml.setId(BC_DAML);
        when(blockchainService.get(BC_DAML)).thenReturn(bcdaml);


        ReflectionTestUtils.setField(BlockchainUtils.class,
                "lintEndpoint",
                "https://data.mgmt.cloud.vmware.com/le-mans/v1/streams/ingestion-pipeline-stream");

        ReflectionTestUtils.setField(BlockchainUtils.class,
                "lintAuthBearer",
                "LINT_TEST_KEY");

        Task t = new Task();
        t.setId(TASK_ID);
        t.setState(Task.State.SUCCEEDED);
        t.setMessage("Done");
        t.setResourceId(BC_DAML);
        when(taskService.put(any())).thenReturn(t);
        when(taskService.get(TASK_ID)).thenReturn(t);

        // This creates our default object mapper
        objectMapper = jacksonBuilder.build();

        // Create authorizations for the different users.
        adminAuth = createContext("operator", ORG_ID,
                                  ImmutableList.of(VmbcRoles.SYSTEM_ADMIN, VmbcRoles.ORG_USER),
                                  ImmutableList.of(C2_ID),
                                  ImmutableList.of(BC_DAML), "");

        consortiumAuth = createContext("consortium", ORG_ID,
                                       ImmutableList.of(VmbcRoles.CONSORTIUM_ADMIN, VmbcRoles.ORG_USER),
                                       Collections.emptyList(),
                                       Collections.emptyList(), "");

        userAuth = createContext("operator", ORG_ID,
                                 ImmutableList.of(VmbcRoles.ORG_USER),
                                 ImmutableList.of(C2_ID),
                                 ImmutableList.of(BC_DAML), "");

        user2Auth = createContext("operator", ORG2_ID,
                                  ImmutableList.of(VmbcRoles.ORG_USER),
                                  ImmutableList.of(C3_ID),
                                  Collections.emptyList(), "");

        // Zone returns
        when(zoneService.get(SITE_1)).thenReturn(ZoneTestUtils.getOnpremZone(SITE_1, ORG_ID));

        when(organizationService.get(any(UUID.class))).thenReturn(mock(Organization.class));

        VmcAwsZone vmcAwsZone = new VmcAwsZone();
        vmcAwsZone.setType(Zone.Type.VMC_AWS);
        vmcAwsZone.setId(SITE_2);
        vmcAwsZone.setNetwork(new Zone.Network("name", null, "10.10.10.10", "24", new ArrayList<>()));
        vmcAwsZone.setResourcePool("resource");
        vmcAwsZone.setFolder("folder");
        vmcAwsZone.setDatacenter("dc");
        vmcAwsZone.setStorage("storage");
        vmcAwsZone.setVmcUrl("vmc");
        vmcAwsZone.setCspUrl("csp");
        vmcAwsZone.setRefreshToken("rt");
        vmcAwsZone.setOrganization("org");
        when(zoneService.get(SITE_2)).thenReturn(vmcAwsZone);
    }


    @Test
        void getParticipantNodeList() throws Exception {
        MvcResult result = mockMvc.perform(
              get("/api/blockchains/" + BC_DAML.toString() + "/clients")
              .with(authentication(adminAuth))
              .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();

        String body = result.getResponse().getContentAsString();
        List<ClientController.ClientGetResponse> res =
                    objectMapper.readValue(body, new TypeReference<List<ClientController.ClientGetResponse>>() {
                    });

        ImmutableList<UUID> expected = ImmutableList.of(REPLICA_2, REPLICA_3);
        Assertions.assertEquals(expected, res.stream().map(replicaGetResponse ->
                replicaGetResponse.getId()).collect(Collectors.toList()));
        Assertions.assertEquals(2, res.size());
    }

    @Test
    void getParticipantNodeListViaClient() throws Exception {

        final Client replica1 = new Client("publicIp", "privateIp", "hostName", "url",
                "cert", BC_DAML, REPLICA_1_ZONE, UUID.randomUUID());
        replica1.setId(REPLICA_1);

        when(clientService.getClientsByBlockchainId(BC_DAML)).thenReturn(ImmutableList.of(replica1));
        MvcResult result = mockMvc.perform(
                get("/api/blockchains/" + BC_DAML.toString() + "/clients")
                        .with(authentication(adminAuth))
                        .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();

        String body = result.getResponse().getContentAsString();
        List<ClientController.ClientGetResponse> res =
                objectMapper.readValue(body, new TypeReference<List<ClientController.ClientGetResponse>>() {
                });

        ImmutableList<UUID> expected = ImmutableList.of(REPLICA_1);
        Assertions.assertEquals(expected, res.stream().map(replicaGetResponse ->
                                                                   replicaGetResponse.getId())
                .collect(Collectors.toList()));
        Assertions.assertEquals(1, res.size());
    }

    @Test
    void testGetClientCredentials() throws Exception {
        String url = String.format("/api/blockchains/%s/clients/%s/credentials", BC_DAML.toString(), C2_ID.toString());

        final Client client = new Client("publicIp", "privateIp", "testPassword",
                "url", "cert", BC_DAML, REPLICA_1_ZONE, UUID.randomUUID());
        client.setId(C2_ID);

        when(clientService.getClientsByBlockchainId(BC_DAML)).thenReturn(ImmutableList.of(client));
        MvcResult result = mockMvc.perform(get(url).with(authentication(adminAuth)))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        NodeGetCredentialsResponse clientCredentials =
                objectMapper.readValue(body, new TypeReference<NodeGetCredentialsResponse>() {});
        Assertions.assertEquals("root", clientCredentials.username);
        Assertions.assertEquals("testPassword", clientCredentials.password);
    }
}
