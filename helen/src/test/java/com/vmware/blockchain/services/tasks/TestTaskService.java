/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.tasks;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Consumer;

import com.vmware.blockchain.common.NotFoundException;

/**
 * Simple task service for use in unit tests.
 */
public class TestTaskService implements ITaskService {
    private Map<UUID, Task> taskMap = new HashMap<>();

    @Override
    public Task get(UUID taskId) {
        if (taskMap.containsKey(taskId)) {
            return taskMap.get(taskId);
        } else {
            throw new NotFoundException("Not found");
        }
    }

    @Override
    public Task put(Task task) {
        if (task.getId() == null) {
            task.setId(UUID.randomUUID());
        }
        taskMap.put(task.getId(), task);
        return task;
    }

    @Override
    public Task merge(Task task, Consumer<Task> merger) {
        return put(task);
    }

    @Override
    public List<Task> list() {
        return new ArrayList<>(taskMap.values());
    }

    public void reset() {
        taskMap = new HashMap<>();
    }
}
