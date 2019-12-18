/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.operation;

import java.util.UUID;

import org.apache.logging.log4j.ThreadContext;
import org.springframework.stereotype.Component;


/**
 * Object for OperationContext.
 */

@Component
public class OperationContext {

    private static ThreadLocal<String> id = new InheritableThreadLocal<>();

    /**
     * Initialize, set, get and remove ID.
     */

    public void initId() {
        String id = UUID.randomUUID().toString();
        setId(id);
    }

    public void setId(String id) {
        this.id.set(id);
        ThreadContext.put("OpId", id);
    }

    public String getId() {
        return id.get();
    }

    public void removeId() {
        id.remove();
        ThreadContext.remove("OpId");
    }

}
