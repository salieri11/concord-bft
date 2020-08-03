/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.stream.Collectors;

import javax.validation.ConstraintViolation;
import javax.validation.Validation;
import javax.validation.Validator;

import org.springframework.context.MessageSource;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.castor.model.DeploymentDescriptorModel;
import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

/**
 * Validation Service. Validates the contents of the infra and deployment descriptors.
 */
@Service
@Log4j2
@RequiredArgsConstructor
public class ValidatorServiceImpl implements ValidatorService {

    private final MessageSource messageSource;

    @Override
    public List<ValidationError> validate(
            InfrastructureDescriptorModel infrastructureDescriptor, DeploymentDescriptorModel deploymentDescriptor) {

        List<ValidationError> errors = new ArrayList<>();
        Validator validator = Validation.buildDefaultValidatorFactory().getValidator();

        Set<ConstraintViolation<InfrastructureDescriptorModel>> infraViolations =
                validator.validate(infrastructureDescriptor);
        List<ValidationError> infraErrors = infraViolations.stream()
                .map(cv -> ValidationError.builder()
                        .errorCode(cv.getMessage())
                        .propertyPath(cv.getPropertyPath().toString())
                        .arguments(List.of(String.valueOf(cv.getInvalidValue())))
                        .build())
                .collect(Collectors.toList());
        errors.addAll(infraErrors);

        Set<ConstraintViolation<DeploymentDescriptorModel>> deploymentViolations =
                validator.validate(deploymentDescriptor);
        List<ValidationError> deploymentErrors = deploymentViolations.stream()
                .map(cv -> ValidationError.builder()
                        .errorCode(cv.getMessage())
                        .propertyPath(cv.getPropertyPath().toString())
                        .arguments(List.of(String.valueOf(cv.getInvalidValue())))
                        .build())
                .collect(Collectors.toList());
        errors.addAll(deploymentErrors);

        // Ensure that zones used in deployment are present in infrastructure
        Set<String> allDeploymentZones = new HashSet<>();
        List<String> committers = deploymentDescriptor.getCommitters();
        if (committers != null) {
            allDeploymentZones.addAll(committers);
        }
        List<DeploymentDescriptorModel.Client> clients = deploymentDescriptor.getClients();
        if (clients != null) {
            Set<String> clientZones = deploymentDescriptor.getClients().stream()
                    .map(DeploymentDescriptorModel.Client::getZoneName).collect(Collectors.toSet());
            allDeploymentZones.addAll(clientZones);
        }

        Set<String> allInfraZones = infrastructureDescriptor.getZones().stream()
                .map(InfrastructureDescriptorModel.Zone::getName).collect(Collectors.toSet());

        allDeploymentZones.removeAll(allInfraZones);
        if (!allDeploymentZones.isEmpty()) {
            List<String> missingDeploymentZones = allDeploymentZones.stream().collect(Collectors.toList());
            String error = "deployment.zones.not.present.in.infrastructure";
            ValidationError e = ValidationError.builder()
                    .errorCode(error)
                    .propertyPath("zoneName")
                    .arguments(missingDeploymentZones)
                    .build();
            errors.add(e);
        }

        for (ValidationError ve : errors) {
            Object[] msgArgs = new Object[] {ve.getPropertyPath(), ve.getArguments()};
            String msg = messageSource.getMessage(
                    ve.getErrorCode(), msgArgs, Locale.getDefault());
            log.error(msg);
        }
        return errors;
    }
}
