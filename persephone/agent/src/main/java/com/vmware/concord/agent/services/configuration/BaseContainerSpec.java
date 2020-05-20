/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent.services.configuration;


import java.util.List;

import com.github.dockerjava.api.model.Bind;
import com.github.dockerjava.api.model.Link;
import com.github.dockerjava.api.model.PortBinding;

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

    List<Link> getLinks();

    @Deprecated
    default List<String> getEnvironment() {
        return null;
    }
}
