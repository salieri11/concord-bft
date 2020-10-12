/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.model;

import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;

import lombok.Builder;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

/**
 * Deployment Descriptor for reconfiguration. Contains properties that are specific to each blockchain reconfiguration.
 */
@Getter
@Setter
@Builder
@EqualsAndHashCode
public class ReconfigurationDescriptorModel implements DeploymentDescriptorModel {

    // List of zone ids on which the committers should be deployed
    // These MUST match the zone name in the Infrastructure descriptor.
    @NotEmpty(message = "deployment.commiters.not.specified")
    @Valid
    private List<Committer> committers;

    @Valid
    private NodeSpecification committerNodeSpec;

    @NotEmpty(message = "deployment.clients.not.specified")
    @Valid
    private List<Client> clients;

    @Valid
    private NodeSpecification clientNodeSpec;

    @Valid
    private Blockchain blockchain;
}
