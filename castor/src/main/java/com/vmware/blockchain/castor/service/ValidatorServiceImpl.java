/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import javax.validation.ConstraintViolation;
import javax.validation.Validation;
import javax.validation.Validator;

import org.springframework.context.MessageSource;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

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
        List<DeploymentDescriptorModel.Committer> committers = deploymentDescriptor.getCommitters();
        if (committers != null) {
            Set<String> committerZones =
                    committers.stream().map(DeploymentDescriptorModel.Committer::getZoneName).collect(
                            Collectors.toSet());
            allDeploymentZones.addAll(committerZones);
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
            List<String> missingDeploymentZones = new ArrayList<>(allDeploymentZones);
            String error = "deployment.zones.not.present.in.infrastructure";
            ValidationError e = ValidationError.builder()
                    .errorCode(error)
                    .propertyPath("zoneName")
                    .arguments(missingDeploymentZones)
                    .build();
            errors.add(e);
        }

        validateIPs(deploymentDescriptor, errors);

        for (ValidationError ve : errors) {
            Object[] msgArgs = new Object[] {ve.getPropertyPath(), ve.getArguments()};
            String msg = messageSource.getMessage(
                    ve.getErrorCode(), msgArgs, Locale.getDefault());
            log.error(msg);
        }

        log.info("Finished validation, found {} errors", errors.size());
        return errors;
    }

    // If VM IPs are specified:
    // (1) they must be specified for ALL nodes (both client and committer,
    // (2) they must all be unique.
    private void validateIPs(
            DeploymentDescriptorModel deploymentDescriptor, List<ValidationError> errors) {

        List<DeploymentDescriptorModel.Client> clients = deploymentDescriptor.getClients();
        clients = Objects.requireNonNullElse(clients, Collections.emptyList());
        List<String> clientProvidedIps =
                clients.stream()
                        .map(DeploymentDescriptorModel.Client::getProvidedIp)
                        .filter(StringUtils::hasText)
                        .collect(Collectors.toList());

        List<DeploymentDescriptorModel.Committer> committers = deploymentDescriptor.getCommitters();
        committers = Objects.requireNonNullElse(committers, Collections.emptyList());
        List<String> committerProvidedIps = committers.stream()
                .map(DeploymentDescriptorModel.Committer::getProvidedIp)
                .filter(StringUtils::hasText)
                .collect(Collectors.toList());

        // Verify that all committers have IPs, or none do
        int numProvidedIps = clientProvidedIps.size() + committerProvidedIps.size();
        int numNodes = clients.size() + committers.size();
        if (numProvidedIps > 0 && numProvidedIps != numNodes) {
            String error = "not.all.ips.specified.for.deployment";
            ValidationError e = ValidationError.builder()
                    .errorCode(error)
                    .propertyPath("providedIp")
                    .build();
            errors.add(e);
        }

        // Verify that if IPs are unique
        Set<String> uniqueIps = new HashSet<>();
        List<String> allProvidedIps = new ArrayList<>();
        uniqueIps.addAll(clientProvidedIps);
        uniqueIps.addAll(committerProvidedIps);
        if (numProvidedIps != uniqueIps.size()) {
            String error = "provided.ips.not.unique";
            ValidationError e = ValidationError.builder()
                    .errorCode(error)
                    .propertyPath("providedIp")
                    .build();
            errors.add(e);
        }

        // DINKARTODO: Do we need to validate that ips provided fall within the subnet for the zone in which
        // the client/committer is placed ?
    }
}
