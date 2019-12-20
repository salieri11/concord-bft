/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.dao;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Linked Entity Id.
 * Linked Entity Id along with current object Id will be stored in {@link Link} table.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface LinkedEntityId {

    /**
     * Whether or not to use bidreictional link.
     */
    boolean biDirectional() default false;
}
