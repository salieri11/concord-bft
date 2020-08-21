/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.status;

import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.Builder;
import lombok.Data;

/**
 * response data class for node status.
 */
@Data
@Builder
public class NodeStatusResponse {

    @JsonProperty("service_status")
    String status;
}
