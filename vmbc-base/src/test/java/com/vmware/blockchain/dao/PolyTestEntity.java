/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.dao;

import static com.fasterxml.jackson.annotation.JsonTypeInfo.As.EXISTING_PROPERTY;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * Test entity for testing polymorphic class (discriminated untions) in the DB.
 */
@EntityColumnName("test.poly")
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true, doNotUseGetters = true)
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = EXISTING_PROPERTY, property = "type",
        visible = true, defaultImpl = PolyTestEntity.class)
@JsonSubTypes({
        @JsonSubTypes.Type(value = PolyTestEntity.StringType.class, name = "STRING_TYPE"),
        @JsonSubTypes.Type(value = PolyTestEntity.IntType.class, name = "INT_TYPE"),
    })

public class PolyTestEntity extends AbstractEntity {

    /**
     * The type of this entity.
     */
    public enum Type {
        STRING_TYPE,
        INT_TYPE
    }

    Type type;

    /**
     * String type.
     */
    @EntityColumnName("test.poly")
    @Data
    @NoArgsConstructor
    public static class StringType extends PolyTestEntity {
        String name;

        public StringType(String name) {
            setType(Type.STRING_TYPE);
            this.name = name;
        }
    }

    /**
     * Int type.
     */
    @EntityColumnName("test.poly")
    @Data
    @NoArgsConstructor
    public static class IntType extends PolyTestEntity {
        int value;

        public IntType(int value) {
            setType(Type.INT_TYPE);
            this.value = value;
        }
    }

}
