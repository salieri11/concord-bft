/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.replicas;

import static com.vmware.blockchain.security.MvcTestSecurityConfig.createContext;
import static com.vmware.blockchain.security.SecurityTestUtils.ORG_ID;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.authentication;
import static org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers.springSecurity;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.stubbing.Answer;
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
import com.vmware.blockchain.deployment.v1.FleetManagementServiceGrpc.FleetManagementServiceStub;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.UpdateInstanceRequest;
import com.vmware.blockchain.deployment.v1.UpdateInstanceResponse;
import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.security.MvcTestSecurityConfig;
import com.vmware.blockchain.security.SecurityTestUtils;
import com.vmware.blockchain.services.blockchains.Blockchain;
import com.vmware.blockchain.services.blockchains.BlockchainController.BlockchainTaskResponse;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaController.ReplicaGetResponse;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaController.ReplicaObserver;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaController.TaskList;
import com.vmware.blockchain.services.concord.ConcordService;
import com.vmware.blockchain.services.profiles.Consortium;
import com.vmware.blockchain.services.profiles.ConsortiumService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.Roles;
import com.vmware.blockchain.services.profiles.User;
import com.vmware.blockchain.services.profiles.UserService;
import com.vmware.blockchain.services.tasks.Task;
import com.vmware.blockchain.services.tasks.Task.State;
import com.vmware.blockchain.services.tasks.TaskController;
import com.vmware.blockchain.services.tasks.TestTaskService;
import com.vmware.concord.Concord.Peer;

@ExtendWith({SpringExtension.class})
@WebMvcTest(controllers = {ReplicaController.class, TaskController.class })
@ContextConfiguration(classes = {MvcTestSecurityConfig.class, MvcConfig.class, ReplicaConfig.class})
@ComponentScan(basePackageClasses = {ReplicaController.class, HelenExceptionHandler.class})
class ReplicaControllerTest {

    private static UUID N1 = UUID.fromString("3e2e5bbe-dd46-4db4-9b83-79004c61c65f");
    private static UUID N2 = UUID.fromString("5860d051-f189-4bfe-966d-35628875b4e4");
    private static UUID N3 = UUID.fromString("8096b245-f8cf-4c6a-846f-bad92641a592");

    private static UUID B1 = UUID.fromString("6d2bc86f-8556-4092-a9d6-5436f6c113d1");
    private static UUID Z1 = UUID.fromString("112e5c35-5d69-474b-a742-62bb3fc9396b");

    @Autowired
    WebApplicationContext context;

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
    OperationContext operationContext;

    @MockBean
    ConcordService concordService;

    TestTaskService taskService;

    @Autowired
    AuthHelper authHelper;

    @MockBean
    FleetManagementServiceStub fleetServiceStub;

    @Autowired
    ReplicaController replicaController;

    private User user;
    private Consortium consortium;
    private ObjectMapper objectMapper;

    private AuthenticationContext adminAuth;
    private AuthenticationContext userAuth;
    private AuthenticationContext user2Auth;

    private void setCallbacks(Answer answer) {
        doAnswer(answer).when(fleetServiceStub)
                .updateInstance(any(UpdateInstanceRequest.class), any(ReplicaObserver.class));
    }

    @BeforeEach
    void setUp() {
        mockMvc = MockMvcBuilders
                .webAppContextSetup(context)
                .apply(springSecurity())
                .build();
        user = SecurityTestUtils.getUser();
        consortium = SecurityTestUtils.getConsortium();
        UUID c1Id = consortium.getId();
        when(userService.getByEmail(user.getEmail())).thenReturn(user);
        when(consortiumService.get(c1Id)).thenReturn(consortium);

        Blockchain b = Blockchain.builder()
                .consortium(consortium.getId())
                .nodeList(Stream.of(N1, N2, N3)
                                  .map(u -> new Blockchain.NodeEntry(u, u.toString(), "", "", null))
                                  .collect(Collectors.toList()))
                .build();
        b.setId(B1);
        when(blockchainService.get(B1)).thenReturn(b);

        // mini task service
        taskService = new TestTaskService();

        userAuth = createContext("operator", ORG_ID,
                                 ImmutableList.of(Roles.ORG_USER),
                                 ImmutableList.of(c1Id),
                                 ImmutableList.of(B1), "");

        adminAuth = createContext("operator", ORG_ID,
                                  ImmutableList.of(Roles.CONSORTIUM_ADMIN, Roles.ORG_USER),
                                  ImmutableList.of(c1Id),
                                  ImmutableList.of(B1), "");


        // This creates our default object mapper
        objectMapper = jacksonBuilder.build();

        // Set assorted mocked fields in the controller
        ReflectionTestUtils.setField(replicaController, "client", fleetServiceStub);
        ReflectionTestUtils.setField(replicaController, "taskService", taskService);
    }

    @Test
    void nodeAction() throws Exception {
        setCallbacks(i -> {
            ReplicaObserver n = i.getArgument(1);
            n.onNext(UpdateInstanceResponse.newBuilder()
                    .setHeader(MessageHeader.newBuilder()
                            .setId("done")
                            .build())
                    .build());
            n.onCompleted();
            return null;
        });

        String url = "/api/blockchains/6d2bc86f-8556-4092-a9d6-5436f6c113d1"
                     + "/replicas/3e2e5bbe-dd46-4db4-9b83-79004c61c65f?action=stop";
        MvcResult result = mockMvc.perform(post(url).with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isAccepted()).andReturn();
        String body = result.getResponse().getContentAsString();
        BlockchainTaskResponse response = objectMapper.readValue(body, BlockchainTaskResponse.class);
        Task task = taskService.get(response.getTaskId());
        Assertions.assertEquals(State.SUCCEEDED, task.getState());
        Assertions.assertEquals("Operation Complete", task.getMessage());
        Assertions.assertEquals(1, taskService.list().size());
    }

    @Test
    void nodeBadAction() throws Exception {
        String url = "/api/blockchains/6d2bc86f-8556-4092-a9d6-5436f6c113d1"
                     + "/replicas/3e2e5bbe-dd46-4db4-9b83-79004c61c65f?action=goaway";
        mockMvc.perform(post(url).with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest());
    }

    @Test
    void nodeNotAuthorized() throws Exception {
        String url = "/api/blockchains/6d2bc86f-8556-4092-a9d6-5436f6c113d1"
                     + "/replicas/3e2e5bbe-dd46-4db4-9b83-79004c61c65f?action=stop";
        mockMvc.perform(post(url).with(authentication(userAuth))
                                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isForbidden());
    }

    @Test
    void nodeBadNode() throws Exception {
        String url = "/api/blockchains/6d2bc86f-8556-4092-a9d6-5436f6c113d1"
                     + "/replicas/ea5b9115-2ee3-4c34-ba9c-4a0e75d38785?action=stop";
        mockMvc.perform(post(url).with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest());
    }

    @Test
    void nodeActionFail() throws Exception {
        setCallbacks(i -> {
            ReplicaObserver n = i.getArgument(1);
            n.onNext(UpdateInstanceResponse.newBuilder()
                    .setHeader(MessageHeader.newBuilder()
                            .setId("done")
                            .build())
                    .build());

            n.onError(new Exception("oof"));

            return null;
        });

        String url = "/api/blockchains/6d2bc86f-8556-4092-a9d6-5436f6c113d1"
                     + "/replicas/3e2e5bbe-dd46-4db4-9b83-79004c61c65f?action=stop";
        MvcResult result = mockMvc.perform(post(url).with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isAccepted()).andReturn();
        String body = result.getResponse().getContentAsString();
        BlockchainTaskResponse response = objectMapper.readValue(body, BlockchainTaskResponse.class);
        Task task = taskService.get(response.getTaskId());
        Assertions.assertEquals(State.FAILED, task.getState());
        Assertions.assertEquals("oof", task.getMessage());
    }

    @Test
    void nodeListAction() throws Exception {
        setCallbacks(i -> {
            ReplicaObserver n = i.getArgument(1);
            n.onNext(UpdateInstanceResponse.newBuilder()
                    .setHeader(MessageHeader.newBuilder()
                            .setId("done")
                            .build())
                    .build());
            n.onCompleted();
            return null;
        });

        String url = "/api/blockchains/6d2bc86f-8556-4092-a9d6-5436f6c113d1/replicas?action=stop";
        String postBody = "    {"
                      + "        \"node_ids\": ["
                      + "                \"3e2e5bbe-dd46-4db4-9b83-79004c61c65f\","
                      + "                \"5860d051-f189-4bfe-966d-35628875b4e4\","
                      + "                \"8096b245-f8cf-4c6a-846f-bad92641a592\""
                      + "                ]"
                      + "    }";
        MvcResult result = mockMvc.perform(post(url).with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(postBody).characterEncoding("utf-8"))
                .andExpect(status().isAccepted()).andReturn();
        String body = result.getResponse().getContentAsString();
        TaskList taskList = objectMapper.readValue(body, TaskList.class);
        for (UUID tid : taskList.getTaskIds()) {
            Task task = taskService.get(tid);
            Assertions.assertEquals(State.SUCCEEDED, task.getState());
            Assertions.assertEquals("Operation Complete", task.getMessage());
        }
        Assertions.assertEquals(3, taskService.list().size());
    }

    @Test
    void replicaListAction() throws Exception {
        setCallbacks(i -> {
            ReplicaObserver n = i.getArgument(1);
            n.onNext(UpdateInstanceResponse.newBuilder()
                    .setHeader(MessageHeader.newBuilder()
                            .setId("done")
                            .build())
                    .build());

            n.onCompleted();
            return null;
        });

        String url = "/api/blockchains/6d2bc86f-8556-4092-a9d6-5436f6c113d1/replicas?action=stop";
        String postBody = "    {"
                          + "        \"replica_ids\": ["
                          + "                \"3e2e5bbe-dd46-4db4-9b83-79004c61c65f\","
                          + "                \"5860d051-f189-4bfe-966d-35628875b4e4\","
                          + "                \"8096b245-f8cf-4c6a-846f-bad92641a592\""
                          + "                ]"
                          + "    }";
        MvcResult result = mockMvc.perform(post(url).with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(postBody).characterEncoding("utf-8"))
                .andExpect(status().isAccepted()).andReturn();
        String body = result.getResponse().getContentAsString();
        TaskList taskList = objectMapper.readValue(body, TaskList.class);
        for (UUID tid : taskList.getTaskIds()) {
            Task task = taskService.get(tid);
            Assertions.assertEquals(State.SUCCEEDED, task.getState());
            Assertions.assertEquals("Operation Complete", task.getMessage());
        }
        Assertions.assertEquals(3, taskService.list().size());
    }

    @Test
    void testGetReplicas() throws Exception {
        String url = "/api/blockchains/6d2bc86f-8556-4092-a9d6-5436f6c113d1/replicas";
        when(blockchainService.getReplicas(B1)).thenReturn(nodeEntries());
        when(concordService.getMembers(B1)).thenReturn(peerEntries());
        MvcResult result = mockMvc.perform(get(url).with(authentication(adminAuth)))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        List<ReplicaGetResponse> replicas =
                objectMapper.readValue(body, new TypeReference<List<ReplicaGetResponse>>() {});
        Assertions.assertEquals(2, replicas.size());

    }

    private List<Replica> nodeEntries() {
        Replica r1 = new Replica("1.2.3.4", "4.3.2.1", "localhost", "http://localhost", "cert1", Z1,
                Replica.ReplicaType.NONE, B1);
        r1.setId(N1);
        Replica r2 = new Replica("1.2.3.5", "4.3.2.2", "localhost", "http://localhost", "cert2", Z1,
                Replica.ReplicaType.NONE, B1);
        r2.setId(N2);
        return ImmutableList.of(r1, r2);
    }

    private List<Peer> peerEntries() {
        Peer p1 = Peer.newBuilder().setAddress("4.3.2.1").setHostname("replica1").setStatus("live").build();
        Peer p2 = Peer.newBuilder().setAddress("4.3.2.2").setHostname("replica2").setStatus("live").build();
        return ImmutableList.of(p1, p2);
    }
}