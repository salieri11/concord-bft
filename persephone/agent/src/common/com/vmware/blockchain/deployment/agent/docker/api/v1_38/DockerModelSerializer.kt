/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent.docker.api.v1_38

import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.Address
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.AuthConfig
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.ContainerConfig
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.ContainerCreateResponse
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.ContainerInspectResponse
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.ContainerSummary
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.Empty
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.EndpointIPAMConfig
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.EndpointSettings
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.ErrorResponse
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.GraphDriverData
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.HealthConfig
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.HostConfig
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.Image
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.ImageSummary
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.Ipam
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.Mount
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.MountPoint
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.Network
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.NetworkConfig
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.NetworkContainer
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.NetworkCreateResponse
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.NetworkSettings
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.Port
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.PortBinding
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.PortMap
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.RestartPolicy
import com.vmware.blockchain.deployment.http.JsonSerializer
import kotlinx.serialization.modules.serializersModuleOf
import kotlinx.serialization.serializer

/**
 * [JsonSerializer] of all serializable types related to Docker Engine API data models.
 */
object DockerModelSerializer : JsonSerializer(
        serializersModuleOf(
                mapOf(
                        // Default types.
                        String::class to String.serializer(),
                        Empty::class to Empty.serializer(),

                        // Docker API models.
                        Address::class to Address.serializer(),
                        AuthConfig::class to AuthConfig.serializer(),
                        ContainerConfig::class to ContainerConfig.serializer(),
                        ContainerCreateResponse::class to ContainerCreateResponse.serializer(),
                        ContainerInspectResponse::class to ContainerInspectResponse.serializer(),
                        ContainerSummary::class to ContainerSummary.serializer(),
                        EndpointIPAMConfig::class to EndpointIPAMConfig.serializer(),
                        EndpointSettings::class to EndpointSettings.serializer(),
                        ErrorResponse::class to ErrorResponse.serializer(),
                        GraphDriverData::class to GraphDriverData.serializer(),
                        HealthConfig::class to HealthConfig.serializer(),
                        HostConfig::class to HostConfig.serializer(),
                        Image::class to Image.serializer(),
                        ImageSummary::class to ImageSummary.serializer(),
                        Ipam::class to Ipam.serializer(),
                        Mount::class to Mount.serializer(),
                        MountPoint::class to MountPoint.serializer(),
                        Network::class to Network.serializer(),
                        NetworkConfig::class to NetworkConfig.serializer(),
                        NetworkContainer::class to NetworkContainer.serializer(),
                        NetworkCreateResponse::class to NetworkCreateResponse.serializer(),
                        NetworkSettings::class to NetworkSettings.serializer(),
                        Port::class to Port.serializer(),
                        PortBinding::class to PortBinding.serializer(),
                        PortMap::class to PortMap.serializer(),
                        RestartPolicy::class to RestartPolicy.serializer()
                )
        )
)
