/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.model.vsphere;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

import lombok.Builder;
import lombok.Data;

/**
 * Data class.
 */
@Data
@Builder
@JsonPropertyOrder({"atClass", "type"})
public class OvfParameter {

    @JsonProperty("@class")
    String atClass;
    String type;
    OvfProperty[] properties;

    @JsonIgnore
    String selectedKey;

    /**
     * Data class.
     */
    public enum OvfParameterTypes {
        PROPERTY_PARAMS("com.vmware.vcenter.ovf.property_params", "PropertyParams"),
        DEPLOYMENT_OPTION_PARAMS("com.vmware.vcenter.ovf.deployment_option_params",
                                 "DeploymentOptionParams");

        public String classValue;
        public String type;

        OvfParameterTypes(String classValue, String type) {
            this.classValue = classValue;
            this.type = type;
        }
    }
}