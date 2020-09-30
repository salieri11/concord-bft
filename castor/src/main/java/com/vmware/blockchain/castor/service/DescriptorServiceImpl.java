/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.io.IOException;
import java.io.Reader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

import org.springframework.stereotype.Service;

import com.google.gson.FieldNamingPolicy;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonIOException;
import com.google.gson.JsonSyntaxException;
import com.vmware.blockchain.castor.exception.CastorException;
import com.vmware.blockchain.castor.exception.ErrorCode;
import com.vmware.blockchain.castor.model.DeploymentDescriptorModel;
import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;
import com.vmware.blockchain.castor.model.ProvisionDescriptorDescriptorModel;
import com.vmware.blockchain.castor.model.ReconfigurationDescriptorModel;

import lombok.extern.log4j.Log4j2;

/**
 * Infra and Deployment Descriptor services.
 */
@Service
@Log4j2
public class DescriptorServiceImpl implements DescriptorService {

    // Key: Deployment type, Value: the model to serialize/deserialize the payload into
    private final Map<CastorDeploymentType, Class<? extends DeploymentDescriptorModel>> typeToDeploymentDescriptorMap;
    private final Gson gson;

    /**
     * Construct a descriptor service that serializes/deserializes the models.
     */
    public DescriptorServiceImpl() {
        gson = new GsonBuilder().setFieldNamingPolicy(FieldNamingPolicy.IDENTITY).create();

        typeToDeploymentDescriptorMap = new HashMap<>();
        typeToDeploymentDescriptorMap.put(CastorDeploymentType.PROVISION, ProvisionDescriptorDescriptorModel.class);
        typeToDeploymentDescriptorMap.put(CastorDeploymentType.RECONFIGURE, ReconfigurationDescriptorModel.class);
    }

    /**
     * Read and convert to a model, then descriptor file.
     * @param infrastructureDescriptorLocation the path to the infra descriptor
     * @return the infra descriptor model
     */
    public InfrastructureDescriptorModel readInfrastructureDescriptorSpec(String infrastructureDescriptorLocation) {
        Path path = Path.of(infrastructureDescriptorLocation);

        if (!Files.exists(path)) {
            log.error("Infrastructure descriptor file {} is not available", path);
            throw new CastorException(ErrorCode.INFRA_DESC_FILE_MISSING, path);
        }

        try (Reader fileReader = Files.newBufferedReader(path)) {
            InfrastructureDescriptorModel model = gson.fromJson(fileReader, InfrastructureDescriptorModel.class);
            return model;
        } catch (IOException | JsonIOException | JsonSyntaxException e) {
            log.error("Infrastructure descriptor file {} could not be processed", path, e);
            throw new CastorException(ErrorCode.INFRA_DESC_FILE_READ_ERROR, path);
        }
    }

    /**
     * Read and convert to a model, then descriptor file.
     *
     * @param castorDeploymentType The type of deployment: provisioning, deprovisioning, or reconfiguration
     * @param deploymentDescriptorLocation the path to the deployment descriptor
     * @return the deployment descriptor model
     */
    public DeploymentDescriptorModel readDeploymentDescriptorSpec(
            CastorDeploymentType castorDeploymentType, String deploymentDescriptorLocation) {
        Path path = Path.of(deploymentDescriptorLocation);

        Class<? extends DeploymentDescriptorModel> deploymentDescModelClass =
                typeToDeploymentDescriptorMap.get(castorDeploymentType);

        if (!Files.exists(path)) {
            log.error("Deployment descriptor file {} is not available", path);
            throw new CastorException(ErrorCode.DEPL_DESC_FILE_MISSING, path);
        }

        try (Reader fileReader = Files.newBufferedReader(path)) {
            DeploymentDescriptorModel model = gson.fromJson(fileReader, deploymentDescModelClass);
            return model;
        } catch (IOException | JsonIOException | JsonSyntaxException e) {
            log.error("Deployment descriptor file {} could not be processed", path, e);
            throw new CastorException(ErrorCode.DEPL_DESC_FILE_READ_ERROR, path);
        }
    }
}
