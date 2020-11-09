/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.util.List;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

import com.vmware.blockchain.castor.model.DeploymentDescriptorModel;

/**
 * Validator for blockchain types.
 */
public class LedgerTlsClientAuthValidator
        implements ConstraintValidator<LedgerTlsClientAuthValid, DeploymentDescriptorModel.LedgerTls.ClientAuth> {

    private DeploymentDescriptorModel.LedgerTls.ClientAuth[] allowedTypes;

    public void initialize(LedgerTlsClientAuthValid constraint) {
        allowedTypes = constraint.allowedTypes();
    }

    @Override
    public boolean isValid(
            DeploymentDescriptorModel.LedgerTls.ClientAuth clientAuth,
            ConstraintValidatorContext constraintValidatorContext) {
        return clientAuth != null && List.of(allowedTypes).contains(clientAuth);
    }
}
