/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.tasks;

import java.util.List;
import java.util.UUID;
import java.util.function.Consumer;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.dao.GenericDao;

/**
 * Perform any businees logic for a task.  At this point, there isn't much.
 */
@Service
public class TaskService {

    private GenericDao genericDao;

    @Autowired
    public TaskService(GenericDao genericDao) {
        this.genericDao = genericDao;
    }

    public Task get(UUID taskId) {
        return genericDao.getEntityByTenant(taskId, Task.class);
    }

    public Task put(Task task) {
        return genericDao.putUnderTenant(task, null);
    }

    public Task merge(Task task, Consumer<Task> merger) {
        return genericDao.mergeWithRetry(task, Task.class, merger);
    }

    public List<Task> list() {
        return genericDao.getAllByType(Task.class);
    }
}
