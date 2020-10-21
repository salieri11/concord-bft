/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.configuration;


import java.util.List;

import com.github.dockerjava.api.model.Bind;
import com.github.dockerjava.api.model.Link;
import com.github.dockerjava.api.model.PortBinding;
import com.github.dockerjava.api.model.RestartPolicy;

/**
 * Interface exposing methods on container component.
 */
public interface BaseContainerSpec {

    int ordinal();

    String getImageId();

    void setImageId(String imageId);

    String getContainerName();

    List<PortBinding> getPortBindings();

    List<Bind> getVolumeBindings();

    @Deprecated
    void setVolumeBindings(List<Bind> input);

    List<Link> getLinks();

    @Deprecated
    default List<String> getEnvironment() {
        return null;
    }

    default RestartPolicy getRestartPolicy() {
        return RestartPolicy.onFailureRestart(5);
    }

    default Long getMemory() {
        return null;
    }
}
