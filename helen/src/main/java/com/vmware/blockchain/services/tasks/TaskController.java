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
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Handle all rest calls for Tasks.
 */
@RestController
public class TaskController {

    @Data
    @NoArgsConstructor
    private static class TaskGetResponse {
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

    private TaskService taskService;

    @Autowired
    public TaskController(TaskService taskService) {
        this.taskService = taskService;
    }

    /**
     * List all tasks.
     * @return List of tasks.
     */
    @RequestMapping(path = "/api/tasks", method = RequestMethod.GET)
    public ResponseEntity<List<TaskGetResponse>> listTasks() {
        List<Task> tasks = taskService.list();
        List<TaskGetResponse> response = tasks.stream().map(TaskGetResponse::new).collect(Collectors.toList());
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    @RequestMapping(path = "/api/tasks/{id}", method = RequestMethod.GET)
    public ResponseEntity<TaskGetResponse> getTask(@PathVariable UUID id) {
        return new ResponseEntity<>(new TaskGetResponse(taskService.get(id)), HttpStatus.OK);
    }

}
