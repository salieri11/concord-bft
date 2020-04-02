/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.model.vsphere;

import static java.util.Collections.emptyList;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Data class.
 */
@Data
public class LibraryItemDeploy {

    /**
     * Data class.
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class LibraryItemDeployResponse {

        LibraryItemDeploymentResult value;
    }

    /**
     * Data class.
     */
    @Data
    @Builder
    public static class LibraryItemDeployRequest {

        LibraryItemDeploymentSpec deploymentSpec;
        LibraryItemDeploymentTarget target;
    }

    /**
     * Data class.
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class LibraryItemDeploymentResult {

        boolean succeeded;
        DeployableIdentity resourceId;
    }

    /**
     * Data class.
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class DeployableIdentity {

        String type;
        String id;
    }

    /**
     * Data class.
     */
    @Data
    @Builder
    public static class LibraryItemDeploymentTarget {

        String resourcePoolId;
        String hostId;
        String folderId;
    }

    /**
     * Data class.
     */
    @Data
    @Builder
    public static class LibraryItemDeploymentSpec {

        @JsonProperty("accept_all_EULA")
        Boolean eula;
        String name;
        String annotation;
        String defaultDatastoreId;
        @Builder.Default
        String storageProvisioning = "thin";
        @Builder.Default
        List<NetworkMapping> networkMappings = emptyList();
        @Builder.Default
        List<OvfParameter> additionalParameters = emptyList();
    }
}