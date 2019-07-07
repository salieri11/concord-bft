/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.tasks;

import java.util.List;
import java.util.UUID;
import java.util.function.Consumer;

/**
 * Interface for task service.  We have two implementations: the real one, and a tests service.
 */
public interface ITaskService {

    Task get(UUID taskId);

    Task put(Task task);

    Task merge(Task task, Consumer<Task> merger);

    List<Task> list();
}
