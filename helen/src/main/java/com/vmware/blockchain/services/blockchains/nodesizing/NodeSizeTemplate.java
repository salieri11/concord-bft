/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.nodesizing;

import static com.fasterxml.jackson.annotation.JsonTypeInfo.As.EXISTING_PROPERTY;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.vmware.blockchain.dao.AbstractEntity;
import com.vmware.blockchain.dao.EntityColumnName;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * Entity class for NodeSizeTemplate.
 */
@EntityColumnName("helen.nodesizetemplate")
@Data
@EqualsAndHashCode(callSuper = true)
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = EXISTING_PROPERTY, property = "type",
        visible = true, defaultImpl = NodeSizeTemplate.class)
public class NodeSizeTemplate extends AbstractEntity {

    // Id
    UUID id;

    // Name
    String name;

    // List of templates
    List<Template> templates;

    // Range map
    Range range;

    /**
     * Default constructor.
     * @param id UUID for the template record.
     * @param name Name for the template record.
     */
    public NodeSizeTemplate(UUID id, String name) {
        this.id = id;
        this.name = name;
    }

    /**
     * Template class.
     * A template is identified by a name, and contains a list of 2 Items, one each of Committer and Client types.
     * An Item is a key value pair of Parameter and String.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class Template {
        String name;
        List<Item> items;
    }

    /**
     * An Item is a key value pair of Parameter and String.
     * For example:
     * 'type' is a key, and the value for it is 'committer'.
     * This item represents a Committer node.
     * {
     *   'type': 'committer'
     *   'no_of_cpus': '4',
     *   'storage_in_gigs': '1024',
     *   'memory_in_gigs': '32'
     *  }
     */
    public static class Item extends LinkedHashMap<Parameter, String> {}

    /**
     * Range is a key value pair of Parameter and RangeVal.
     * For example:
     * 'range':
     *   {
     *     'no_of_cpus': {'min': 1, 'max': 18},
     *     'storage_in_gigs': {'min': 1, 'max': 16384},
     *     'memory_in_gigs': {'min': 1, 'max': 3024}
     *   }
     */
    public static class Range extends LinkedHashMap<Parameter, RangeVal> {}

    /**
     * RangeVal is a key value pair of RangeProperty and Integer.
     * For exammple:
     * {'min': 1, 'max': 16384}
     */
    public static class RangeVal extends LinkedHashMap<RangeProperty, Integer> {}

    /**
     * Enum that has all range type constants.
     */
    public enum Parameter {
        @JsonProperty("no_of_cpus")
        NO_OF_CPUS,
        @JsonProperty("storage_in_gigs")
        STORAGE_IN_GIGS,
        @JsonProperty("memory_in_gigs")
        MEMORY_IN_GIGS,
        @JsonProperty("type")
        TYPE
    }

    /**
     * Enum that represents Range properties.
     */
    public enum RangeProperty {
        @JsonProperty("min")
        MIN,
        @JsonProperty("max")
        MAX
    }
}

