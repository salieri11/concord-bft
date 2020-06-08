/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.tasks;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Handle all rest calls for Tasks.
 */
@RestController
public class TaskController {

    @Data
    @NoArgsConstructor
    static class TaskGetResponse {
        private UUID taskId;
        private Task.State state;
        private String message;
        private UUID resourceId;
        private String resourceLink;

        public TaskGetResponse(Task t) {
            this.taskId = t.getId();
            state = t.getState();
            message = t.getMessage();
            resourceId = t.getResourceId();
            resourceLink = t.getResourceLink();
        }
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    static class TaskListResponse {
        List<TaskGetResponse> states;
    }

    @Data
    static class TaskIdList {
        List<UUID> taskIds;
    }

    private ITaskService taskService;

    @Autowired
    public TaskController(TaskService taskService) {
        this.taskService = taskService;
    }

    /**
     * List all tasks.
     * @return List of tasks.
     */
    @RequestMapping(path = "/api/tasks", method = RequestMethod.GET)
    @PreAuthorize("@authHelper.isSystemAdmin()")
    public ResponseEntity<List<TaskGetResponse>> listTasks() {
        List<Task> tasks = taskService.list();
        List<TaskGetResponse> response = tasks.stream().map(TaskGetResponse::new).collect(Collectors.toList());
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    @RequestMapping(path = "/api/tasks/{id}", method = RequestMethod.GET)
    @PreAuthorize("@authHelper.isAuthenticated()")
    public ResponseEntity<TaskGetResponse> getTask(@PathVariable UUID id) {
        return new ResponseEntity<>(new TaskGetResponse(taskService.get(id)), HttpStatus.OK);
    }

    /**
     * Get the task states from a list of taskIds.  Note that the query param get_states is required
     * by default, so not including it will produce a 400 error.
\     */
    @RequestMapping(path = "/api/tasks", method = RequestMethod.POST, params = {"get_states"})
    @PreAuthorize("@authHelper.isAuthenticated()")
    public ResponseEntity<TaskListResponse> getTaskStates(@RequestBody TaskIdList idList) {
        List<TaskGetResponse> states = idList.getTaskIds().stream()
                .map(taskService::get).map(TaskGetResponse::new)
                .collect(Collectors.toList());
        return new ResponseEntity<>(new TaskListResponse(states), HttpStatus.OK);
    }

}
