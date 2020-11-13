/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import javax.validation.Constraint;
import javax.validation.Payload;

import com.vmware.blockchain.castor.model.DeploymentDescriptorModel;

/**
 * Validation annotation for BlockchainType enum allowed values.
 */
@Target({ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = LedgerTlsClientAuthValidator.class)
public @interface LedgerTlsClientAuthValid {

    /**
     * allowed types.
     * @return allowed types
     */
    DeploymentDescriptorModel.TlsLedgerData.ClientAuth[] allowedTypes();

    /**
     * Default message key.
     * @return message key
     */
    String message() default "client.auth.type.invalid";

    /**
     * Validation grouping.
     * @return validation grouping
     */
    Class<?>[] groups() default {};

    /**
     * Payload.
     * @return payload
     */
    Class<? extends Payload>[] payload() default {};
}
