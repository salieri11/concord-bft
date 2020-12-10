/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.model;

import java.util.List;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotBlank;

import lombok.Builder;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

/**
 * The interface that defines contracts for the provision, deprovision, and reconfigure models.
 */
public interface ReplicaDescriptorModel {

    /**
     * Required replica.
     */
    @Getter
    @Setter
    @SuperBuilder
    @EqualsAndHashCode
    public static class Replica {
        // This MUST match the zone name in the Infrastructure descriptor.
        @NotBlank(message = "deployment.replica.zone.invalid")
        private String zoneName;
        private String providedIp;
    }

    /**
     * Optional read-only replica. Describes a read-only replica that has all the properties of a real replica,
     * and also additional properties specific to read-only replicas.
     */
    @Getter
    @Setter
    @SuperBuilder
    @EqualsAndHashCode
    public static class ReadonlyReplica extends Replica {
        @NotBlank(message = "deployment.roreplica.access.key.invalid")
        private String accessKey;
        @NotBlank(message = "deployment.roreplica.bucket.name.invalid")
        private String bucketName;
        @NotBlank(message = "deployment.roreplica.protocol.invalid")
        private String protocol;
        @NotBlank(message = "deployment.roreplica.secret.key.invalid")
        private String secretKey;
        @NotBlank(message = "deployment.roreplica.url.invalid")
        private String url;
    }

    /**
     * Blockchain operator attributes.
     */
    @Getter
    @Setter
    @SuperBuilder
    @EqualsAndHashCode
    class OperatorSpecifications {
        private String operatorPublicKey;
    }

    /**
     * Defines node size.
     */
    @Getter
    @Setter
    @Builder
    @EqualsAndHashCode
    public static class NodeSpecification {

        @Min(value = 2, message = "invalid.mincpu")
        @Max(value = 32, message = "invalid.maxcpu")
        private int cpuCount;

        @Min(value = 16, message = "invalid.minmemory")
        @Max(value = 128, message = "invalid.maxmemory")
        private int memoryGb;

        @Min(value = 64, message = "invalid.mindisk")
        @Max(value = 62 * 4096, message = "invalid.maxdisk")
        private int diskSizeGb;
    }

    /**
     * Get replica node specification from deployment model.
     * @return node spec
     */
    NodeSpecification getReplicaNodeSpec();

    /**
     * Get replicas from the deployment model.
     * @return a list of replicas
     */
    List<Replica> getReplicas();

    /**
     * Get read-only replica node specification from deployment model.
     * @return node spec
     */
    NodeSpecification getReadonlyReplicaNodeSpec();

    /**
     * Get read-only replicas from the deployment model.
     * @return a list of read-only replicas
     */
    List<ReadonlyReplica> getReadonlyReplicas();

    /**
     * Get the attributes for Blockchain operator.
     * @return a spec
     */
    OperatorSpecifications getOperatorSpecifications();

}
