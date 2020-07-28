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
public class BlockchainTypesValidator
        implements ConstraintValidator<BlockchainTypesValid, DeploymentDescriptorModel.BlockchainType> {

    private DeploymentDescriptorModel.BlockchainType[] allowedTypes;

    public void initialize(BlockchainTypesValid constraint) {
        allowedTypes = constraint.allowedTypes();
    }

    @Override
    public boolean isValid(
            DeploymentDescriptorModel.BlockchainType blockchainType,
            ConstraintValidatorContext constraintValidatorContext) {
        return blockchainType != null && List.of(allowedTypes).contains(blockchainType);
    }
}
