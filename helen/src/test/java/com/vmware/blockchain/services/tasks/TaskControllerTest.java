/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.tasks;

import static com.vmware.blockchain.security.MvcTestSecurityConfig.createContext;
import static com.vmware.blockchain.security.SecurityTestUtils.ORG_ID;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.authentication;
import static org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers.springSecurity;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

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
import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.common.HelenExceptionHandler;
import com.vmware.blockchain.security.MvcTestSecurityConfig;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.VmbcRoles;
import com.vmware.blockchain.services.tasks.Task.State;
import com.vmware.blockchain.services.tasks.TaskController.TaskGetResponse;
import com.vmware.blockchain.services.tasks.TaskController.TaskListResponse;

@ExtendWith({SpringExtension.class})
@WebMvcTest(controllers = {TaskController.class })
@ContextConfiguration(classes = {MvcTestSecurityConfig.class, MvcConfig.class})
@ComponentScan(basePackageClasses = {TaskController.class, HelenExceptionHandler.class})
class TaskControllerTest {
    private static UUID C1 = UUID.fromString("0c347b09-847a-46a2-8e13-f2699c4fb00e");
    private static UUID B1 = UUID.fromString("6d2bc86f-8556-4092-a9d6-5436f6c113d1");

    @Autowired
    private WebApplicationContext context;

    @Autowired
    private TaskController taskController;

    private ITaskService taskService;

    private MockMvc mockMvc;

    @MockBean
    DefaultProfiles defaultProfiles;

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;

    private AuthenticationContext adminAuth;
    private AuthenticationContext userAuth;

    private ObjectMapper objectMapper;

    @BeforeEach
    void setUp() {
        mockMvc = MockMvcBuilders
                .webAppContextSetup(context)
                .apply(springSecurity())
                .build();

        // This creates our default object mapper
        objectMapper = jacksonBuilder.build();

        taskService = new TestTaskService();

        userAuth = createContext("operator", ORG_ID,
                                 ImmutableList.of(VmbcRoles.ORG_USER),
                                 ImmutableList.of(C1),
                                 ImmutableList.of(B1), "");

        adminAuth = createContext("operator", ORG_ID,
                                  ImmutableList.of(VmbcRoles.SYSTEM_ADMIN, VmbcRoles.ORG_USER),
                                  ImmutableList.of(C1),
                                  ImmutableList.of(B1), "");

        ReflectionTestUtils.setField(taskController, "taskService", taskService);
    }

    @Test
    void listTasks() throws Exception {
        // just create some tasks
        List<Task> l = taskList(3);

        MvcResult result = mockMvc.perform(get("/api/tasks").with(authentication(adminAuth)))
                                    .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        List<TaskGetResponse> tasks = objectMapper.readValue(body, new TypeReference<List<TaskGetResponse>>() {});
        Assertions.assertEquals(3, tasks.size());
        Assertions.assertTrue(tasks.stream().allMatch(t -> State.RUNNING == t.getState()));
        Set<UUID> expectedIds = l.stream().map(t -> t.getId()).collect(Collectors.toSet());
        Set<UUID> actualIds = taskService.list().stream().map(t -> t.getId()).collect(Collectors.toSet());
        Assertions.assertEquals(expectedIds, actualIds);
    }

    void listTasksNoAuth() throws Exception {
        // just create some tasks
        List<Task> l = taskList(3);

        mockMvc.perform(get("/api/tasks").with(authentication(adminAuth))).andExpect(status().isForbidden());
    }

    @Test
    void getTask() throws Exception {
        Task t = createTask();
        MvcResult result = mockMvc.perform(get("/api/tasks/" + t.getId().toString()).with(authentication(userAuth)))
                            .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        TaskGetResponse r = objectMapper.readValue(body, TaskGetResponse.class);
        Assertions.assertEquals(r.getTaskId(), t.getId());
        Assertions.assertEquals(State.RUNNING, t.getState());
    }

    @Test
    void getTaskStates() throws Exception {
        List<Task> l = taskList(3);
        String requestBody = String.format("{\"task_ids\": [\"%s\", \"%s\", \"%s\"]}",
                l.get(0).getId(), l.get(1).getId(), l.get(2).getId());

        MvcResult result = mockMvc.perform(post("/api/tasks?get_states").with(authentication(userAuth))
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(requestBody))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        TaskListResponse response = objectMapper.readValue(body, TaskListResponse.class);
        Assertions.assertEquals(3, response.getStates().size());
        Assertions.assertTrue(response.getStates().stream().allMatch(t -> State.RUNNING == t.getState()));
        Set<UUID> expectedIds = l.stream().map(t -> t.getId()).collect(Collectors.toSet());
        Set<UUID> actualIds = taskService.list().stream().map(t -> t.getId()).collect(Collectors.toSet());
        Assertions.assertEquals(expectedIds, actualIds);
    }

    @Test
    void getTaskStatesNoQuery() throws Exception {
        List<Task> l = taskList(3);
        String requestBody = String.format("{\"task_ids\": [\"%s\", \"%s\", \"%s\"]}",
                                           l.get(0).getId(), l.get(1).getId(), l.get(2).getId());

        mockMvc.perform(post("/api/tasks").with(authentication(userAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(requestBody))
                .andExpect(status().isBadRequest());
    }

    // create a test task, and register it.
    private Task createTask() {
        Task t = new Task();
        t.setState(State.RUNNING);
        return taskService.put(t);
    }

    private List<Task> taskList(int n) {
        return IntStream.range(0, n).mapToObj(i -> createTask()).collect(Collectors.toList());
    }
}