/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.clients;

import static com.vmware.blockchain.security.MvcTestSecurityConfig.createContext;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
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
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.MediaType;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
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
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.deployment.v1.CreateClusterRequest;
import com.vmware.blockchain.deployment.v1.DeploymentSessionIdentifier;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceGrpc;
import com.vmware.blockchain.deployment.v1.StreamClusterDeploymentSessionEventRequest;
import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.security.MvcTestSecurityConfig;
import com.vmware.blockchain.security.SecurityTestUtils;
import com.vmware.blockchain.services.blockchains.Blockchain;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.blockchains.replicas.Replica;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;
import com.vmware.blockchain.services.blockchains.zones.VmcAwsZone;
import com.vmware.blockchain.services.blockchains.zones.Zone;
import com.vmware.blockchain.services.blockchains.zones.ZoneService;
import com.vmware.blockchain.services.blockchains.zones.ZoneTestUtils;
import com.vmware.blockchain.services.configuration.ConcordConfiguration;
import com.vmware.blockchain.services.profiles.Consortium;
import com.vmware.blockchain.services.profiles.ConsortiumService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.Organization;
import com.vmware.blockchain.services.profiles.OrganizationService;
import com.vmware.blockchain.services.profiles.Roles;
import com.vmware.blockchain.services.profiles.User;
import com.vmware.blockchain.services.profiles.UserService;
import com.vmware.blockchain.services.tasks.Task;
import com.vmware.blockchain.services.tasks.TaskController;
import com.vmware.blockchain.services.tasks.TaskService;

import io.grpc.stub.StreamObserver;


/**
 * Tests for the client controller.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:lint-test.properties")
@WebMvcTest(controllers = { ClientController.class, TaskController.class })
@ContextConfiguration(classes = {MvcTestSecurityConfig.class, MvcConfig.class})
@ComponentScan(basePackageClasses = { ClientControllerTest.class, HelenExceptionHandler.class,
                                      TaskController.class, ConcordConfiguration.class })
public class ClientControllerTest extends RuntimeException {

    static final UUID BC_ID = UUID.fromString("437d97b2-76df-4596-b0d8-3d8a9412ff2f");
    static final UUID BC2_ID = UUID.fromString("7324cb8f-0ffc-4311-b57e-4c3e1e10a3aa");
    static final UUID BC_NEW = UUID.fromString("4b8a5ec6-91ad-437d-b574-45f5b7345b96");
    static final UUID BC_DAML = UUID.fromString("fd7167b0-057d-11ea-8d71-362b9e155667");
    static final UUID BC_DAML_GET_CLIENT = UUID.fromString("ded15efc-33e3-11ea-978f-2e728ce88125");
    static final UUID C2_ID = UUID.fromString("04e4f62d-5364-4363-a582-b397075b65a3");
    static final UUID C3_ID = UUID.fromString("a4b8f7ed-00b3-451e-97bc-4aa51a211288");
    static final UUID TASK_ID = UUID.fromString("c23ed97d-f29c-472e-9f63-cc6be883a5f5");
    static final UUID ORG_ID = UUID.fromString("5c373085-0cd1-47e4-b4f2-66d418f22fdf");
    static final UUID ORG2_ID = UUID.fromString("a774d0e3-b182-4330-93df-6738c8b1b2de");
    static final UUID DEP_ID = UUID.fromString("67376aed-333c-4e35-b6b6-c59800752dc3");
    private static final UUID SITE_1 = UUID.fromString("84b9a0ed-c162-446a-b8c0-2e45755f3844");
    private static final UUID SITE_2 = UUID.fromString("275638a3-8860-4925-85de-c73d45cb7232");
    private static final UUID NODE_1 = UUID.fromString("f81899ce-861f-4479-9adf-f3ad753fcaf6");
    private static final UUID NODE_2 = UUID.fromString("81a70aeb-c13c-4f36-9e98-564c1e6eccdc");
    private static final UUID REPLICA_1 = UUID.fromString("987ec776-679f-4428-9135-4db872a0a64b");
    private static final UUID REPLICA_2 = UUID.fromString("2462e0bf-ccbd-4e7b-b5ee-70514d3188cf");
    private static final UUID REPLICA_3 = UUID.fromString("e10a68e3-faa5-4ab6-a69f-53e10bc0d490");
    private static final UUID REPLICA_4 = UUID.fromString("493b84d7-5333-4294-bba7-c56ade48cb73");

    // DAML participant
    static final String POST_BODY_DAML_PARTICIPANT = "{"
                                                     + "    \"zone_ids\": ["
                                                     + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                                     + "            \"275638a3-8860-4925-85de-c73d45cb7232\","
                                                     + "            \"275638a3-8860-4925-85de-c73d45cb7232\"]" + "}";

    static final String POST_BODY_UNSP = "{"
                                         + "    \"consortium_id\": \"04e4f62d-5364-4363-a582-b397075b65a3\","
                                         + "    \"f_count\": 1,"
                                         + "    \"c_count\": 0,"
                                         + "    \"deployment_type\": \"UNSPECIFIED\"" + "}";
    @Autowired
    private WebApplicationContext context;

    private MockMvc mockMvc;

    @MockBean
    DefaultProfiles defaultProfiles;

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;

    @MockBean
    UserService userService;

    @MockBean
    ConsortiumService consortiumService;

    @MockBean
    BlockchainService blockchainService;

    @MockBean
    ReplicaService replicaService;

    @MockBean
    ProvisioningServiceGrpc.ProvisioningServiceStub client;

    @MockBean
    OperationContext operationContext;

    @Autowired
    ConcordConfiguration concordConfiguration;

    @MockBean
    OrganizationService organizationService;

    @MockBean
    ZoneService zoneService;

    @Autowired
    TaskService taskService;

    @Autowired
    AuthHelper authHelper;

    private User user;
    private Consortium consortium;
    private ObjectMapper objectMapper;

    private AuthenticationContext adminAuth;
    private AuthenticationContext consortiumAuth;
    private AuthenticationContext userAuth;
    private AuthenticationContext user2Auth;

    private DeploymentSessionIdentifier dsId;

    private void setCreateCluster(Answer answer) {
        doAnswer(answer).when(client)
                .createCluster(any(CreateClusterRequest.class), any(StreamObserver.class));
    }

    private void setStreamCluster(Answer answer) {
        doAnswer(answer).when(client)
                .streamClusterDeploymentSessionEvents(any(StreamClusterDeploymentSessionEventRequest.class),
                                                      any(StreamObserver.class));
    }

    /**
     * A mockito Answer that saves the result of the mock, so we can look at it later.
     */
    static class ResultAnswer<T> implements Answer {

        T result;
        Function<InvocationOnMock, T> function;

        public ResultAnswer(Function<InvocationOnMock, T> function) {
            this.function = function;
        }

        public T getResult() {
            return result;
        }

        @Override
        public Object answer(InvocationOnMock invocation) throws Throwable {
            result = function.apply(invocation);
            return result;
        }
    }

    private ResultAnswer<Blockchain> blockchainResultAnswer;

    /**
     * Initialize various mocks.
     */
    @BeforeEach
    public void init() {
        mockMvc = MockMvcBuilders
                .webAppContextSetup(context)
                .apply(springSecurity())
                .build();
        user = SecurityTestUtils.getUser();
        consortium = SecurityTestUtils.getConsortium();
        when(operationContext.getId()).thenReturn(UUID.randomUUID().toString());
        Consortium c2 = new Consortium();
        c2.setId(C2_ID);
        Consortium c3 = new Consortium();
        c3.setId(C3_ID);
        UUID c1Id = consortium.getId();
        when(userService.getByEmail(user.getEmail())).thenReturn(user);
        when(consortiumService.get(c1Id)).thenReturn(consortium);
        when(consortiumService.get(C2_ID)).thenReturn(c2);
        when(consortiumService.get(C3_ID)).thenReturn(c3);

        final Blockchain b = Blockchain.builder()
                .consortium(consortium.getId())
                .nodeList(Stream.of("1", "2", "3")
                                  .map(s -> new Blockchain.NodeEntry(UUID.randomUUID(), s, "", "", SITE_1))
                                  .collect(Collectors.toList()))
                .build();
        final Blockchain b2 = Blockchain.builder()
                .consortium(c2.getId())
                .nodeList(Stream.of("4", "5", "6")
                                  .map(s -> new Blockchain.NodeEntry(UUID.randomUUID(), s, "", "", SITE_2))
                                  .collect(Collectors.toList()))
                .build();
        final Blockchain bn = Blockchain.builder()
                .consortium(UUID.fromString("04e4f62d-5364-4363-a582-b397075b65a3"))
                .nodeList(Stream.of("one", "two", "three")
                                  .map(s -> new Blockchain.NodeEntry(UUID.randomUUID(), s, "http://".concat(s),
                                                                     "cert-".concat(s), SITE_2))
                                  .collect(Collectors.toList()))
                .build();
        final Blockchain bcdaml = Blockchain.builder()
                .consortium(UUID.fromString("5a0cebc0-057e-11ea-8d71-362b9e155667"))
                .nodeList(Stream.of("one", "two", "three")
                                  .map(s -> new Blockchain.NodeEntry(UUID.randomUUID(), s, "http://".concat(s),
                                                                     "cert-".concat(s), SITE_2))
                                  .collect(Collectors.toList()))
                .type(Blockchain.BlockchainType.DAML)
                .build();

        final Replica replica1 = new Replica("publicIp", "privateIp", "hostName", "url", "cert", REPLICA_1,
                                             Replica.ReplicaType.NONE, BC_DAML_GET_CLIENT);

        final Replica replica2 = new Replica("publicIp", "privateIp", "hostName", "url", "cert", REPLICA_2,
                                             Replica.ReplicaType.DAML_PARTICIPANT, BC_DAML_GET_CLIENT);

        final Replica replica3 = new Replica("publicIp", "privateIp", "hostName", "url", "cert", REPLICA_3,
                                             Replica.ReplicaType.DAML_PARTICIPANT, BC_DAML_GET_CLIENT);

        final Replica replica4 = new Replica("publicIp", "privateIp", "hostName", "url", "cert", REPLICA_4,
                                             Replica.ReplicaType.NONE, BC_DAML_GET_CLIENT);

        when(blockchainService.getReplicas(BC_DAML_GET_CLIENT))
                .thenReturn(ImmutableList.of(replica1, replica2, replica3, replica4));

        when(blockchainService.getReplicas(BC_DAML_GET_CLIENT))
                .thenReturn(ImmutableList.of(replica1, replica2, replica3, replica4));
        when(blockchainService.listByConsortium(consortium)).thenReturn(Collections.singletonList(b));
        when(blockchainService.listByConsortium(c3)).thenReturn(Collections.emptyList());
        when(blockchainService.list()).thenReturn(ImmutableList.of(b, b2));
        b.setId(BC_ID);
        b2.setId(BC2_ID);
        bn.setId(BC_NEW);
        bcdaml.setId(BC_DAML);
        when(blockchainService.get(BC_ID)).thenReturn(b);
        when(blockchainService.get(BC2_ID)).thenReturn(b2);
        when(blockchainService.get(BC_NEW)).thenReturn(bn);
        when(blockchainService.get(BC_DAML)).thenReturn(bcdaml);
        when(blockchainService.get(C2_ID)).thenThrow(new NotFoundException("Not found"));
        when(blockchainService.listByIds(any(List.class))).thenAnswer(i -> {
            return ((List<UUID>) i.getArgument(0)).stream().map(blockchainService::get).collect(Collectors.toList());
        });
        blockchainResultAnswer = new ResultAnswer<>(i -> {
            Blockchain bc = new Blockchain();
            bc.setConsortium(i.getArgument(1));
            bc.setType(i.getArgument(2));
            bc.setNodeList(i.getArgument(3));
            bc.setId(i.getArgument(0));
            return bc;
        });

        when(blockchainService
                     .create(any(UUID.class), any(UUID.class), any(Blockchain.BlockchainType.class), any(List.class)))
                .thenAnswer(blockchainResultAnswer);
        when(defaultProfiles.getBlockchain()).thenReturn(bn);
        Task t = new Task();
        t.setId(TASK_ID);
        t.setState(Task.State.SUCCEEDED);
        t.setMessage("Done");
        t.setResourceId(BC_NEW);
        when(taskService.put(any())).thenReturn(t);
        when(taskService.get(TASK_ID)).thenReturn(t);
        // This creates our default object mapper
        objectMapper = jacksonBuilder.build();

        // Create authorizations for the different users.
        adminAuth = createContext("operator", ORG_ID,
                                  ImmutableList.of(Roles.SYSTEM_ADMIN, Roles.ORG_USER),
                                  ImmutableList.of(C2_ID),
                                  ImmutableList.of(BC_ID), "");

        consortiumAuth = createContext("consortium", ORG_ID,
                                       ImmutableList.of(Roles.CONSORTIUM_ADMIN, Roles.ORG_USER),
                                       Collections.emptyList(),
                                       Collections.emptyList(), "");

        userAuth = createContext("operator", ORG_ID,
                                 ImmutableList.of(Roles.ORG_USER),
                                 ImmutableList.of(C2_ID),
                                 ImmutableList.of(BC_ID), "");

        user2Auth = createContext("operator", ORG2_ID,
                                  ImmutableList.of(Roles.ORG_USER),
                                  ImmutableList.of(C3_ID),
                                  Collections.emptyList(), "");

        dsId = DeploymentSessionIdentifier.newBuilder()
                .setLow(DEP_ID.getLeastSignificantBits())
                .setHigh(DEP_ID.getMostSignificantBits())
                .build();
        setCreateCluster(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(dsId);
            ob.onCompleted();
            return null;
        });

        // Zone returns
        when(zoneService.get(SITE_1)).thenReturn(ZoneTestUtils.getOnpremZone(SITE_1, ORG_ID));

        when(zoneService.get(SITE_2)).thenReturn(new Zone(SITE_2, Zone.Type.VMC_AWS));

        when(organizationService.get(any(UUID.class))).thenReturn(mock(Organization.class));

        VmcAwsZone vmcAwsZone = new VmcAwsZone();
        vmcAwsZone.setType(Zone.Type.VMC_AWS);
        vmcAwsZone.setId(SITE_2);
        vmcAwsZone.setNetwork(new Zone.Network("name", null, "10.10.10.10", "24",
                                               new ArrayList<>()));
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
              get("/api/blockchains/" + BC_DAML_GET_CLIENT.toString() + "/clients")
              .with(authentication(adminAuth))
              .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();

        String body = result.getResponse().getContentAsString();
        List<ClientController.ReplicaGetResponse> res =
                    objectMapper.readValue(body, new TypeReference<List<ClientController.ReplicaGetResponse>>() {
                    });

        Assertions.assertEquals(2, res.size());
        Assertions.assertEquals(Replica.ReplicaType.DAML_PARTICIPANT, res.get(0).getReplicaType());
        Assertions.assertEquals(Replica.ReplicaType.DAML_PARTICIPANT, res.get(1).getReplicaType());
    }

    /**@Test
    void deployParticipant() throws Exception {
        ArgumentCaptor<CreateClusterRequest> captor = ArgumentCaptor.forClass(CreateClusterRequest.class);
        mockMvc.perform(post("/api/blockchains/" + BC_DAML.toString() + "/clients")
                                    .with(authentication(adminAuth))
                                    .contentType(MediaType.APPLICATION_JSON)
                                    .content(POST_BODY_DAML_PARTICIPANT))
                    .andExpect(status().isAccepted());

        verify(client).createCluster(captor.capture(), any(StreamObserver.class));
        CreateClusterRequest request = captor.getValue();
        Assertions.assertEquals(0, request.getSpecification().getClusterSize());
        List<PlacementSpecification.Entry> entries = request.getSpecification().getPlacement().getEntriesList();
        Assertions.assertEquals(3, entries.size());
        Assertions.assertTrue(entries.stream().allMatch(e -> e.getType() == PlacementSpecification.Type.FIXED));
        Assertions.assertEquals(1, entries.stream()
                    .filter(e -> OrchestrationSiteIdentifier.newBuilder()
                            .setLow(SITE_1.getLeastSignificantBits())
                            .setHigh(SITE_1.getMostSignificantBits())
                            .build()
                            .equals(e.getSite()))
                    .count());
        Assertions.assertEquals(2, entries.stream()
                    .filter(e -> OrchestrationSiteIdentifier.newBuilder()
                            .setLow(SITE_2.getLeastSignificantBits())
                            .setHigh(SITE_2.getMostSignificantBits())
                            .build()
                            .equals(e.getSite()))
                    .count());
    }
    */
}
