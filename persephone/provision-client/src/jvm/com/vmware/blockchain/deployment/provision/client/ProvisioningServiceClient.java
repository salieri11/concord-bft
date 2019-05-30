/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.provision.client;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.model.ConcordComponent;
import com.vmware.blockchain.deployment.model.ConcordModelSpecification;
import com.vmware.blockchain.deployment.model.CreateClusterRequest;
import com.vmware.blockchain.deployment.model.DeploymentSessionIdentifier;
import com.vmware.blockchain.deployment.model.DeploymentSpecification;
import com.vmware.blockchain.deployment.model.MessageHeader;
import com.vmware.blockchain.deployment.model.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.model.PlacementSpecification;
import com.vmware.blockchain.deployment.model.PlacementSpecification.Entry;
import com.vmware.blockchain.deployment.model.ProvisioningServiceStub;
import com.vmware.blockchain.deployment.model.ethereum.Genesis;
import com.vmware.blockchain.deployment.provision.common.Observers;
import com.vmware.blockchain.deployment.provision.deletedeployment.ResourceDeprovisioner;

import io.grpc.CallOptions;
import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;

/**
 * Simple gRPC client for accessing Provisioning-Service.
 */
public class ProvisioningServiceClient {

    private static Logger log = LoggerFactory.getLogger(ProvisioningServiceClient.class);
    private static String CREATE_CLUSTER = "createCluster";

    /**
     * The actual call which will contact server and add the model request.
     * TODO: Make more generic taking in the size and move to different file
     */
    private static boolean createFixedSizeCluster(String address) {
        try {
            // Create a channel
            ManagedChannel channel = ManagedChannelBuilder.forTarget(address).usePlaintext().build();


            // Create a blocking stub with the channel
            var client = new ProvisioningServiceStub(channel, CallOptions.DEFAULT);
            var clusterSize = 4;
            var list = List.of(
                    new Entry(PlacementSpecification.Type.FIXED, new OrchestrationSiteIdentifier(4, 0)),
                    new Entry(PlacementSpecification.Type.FIXED, new OrchestrationSiteIdentifier(4, 0)),
                    new Entry(PlacementSpecification.Type.FIXED, new OrchestrationSiteIdentifier(4, 0)),
                    new Entry(PlacementSpecification.Type.FIXED, new OrchestrationSiteIdentifier(4, 0))
            );
            var placementSpec = new PlacementSpecification(list);
            var components = List.of(
                    new ConcordComponent(ConcordComponent.Type.DOCKER_IMAGE, "vmwblockchain/concord-core:latest"),
                    new ConcordComponent(ConcordComponent.Type.DOCKER_IMAGE, "vmwblockchain/ethrpc:latest"),
                    new ConcordComponent(ConcordComponent.Type.DOCKER_IMAGE, "vmwblockchain/agent-testing:latest")
            );
            var genesis = new Genesis(
                    new Genesis.Config(1, 0, 0, 0),
                    "0x0000000000000000",
                    "0x400",
                    "0x0000000000000000000000000000000000000000000000000000000000000000",
                    "0x0000000000000000000000000000000000000000000000000000000000000000",
                    "0xf4240",
                    Map.of(
                            "262c0d7ab5ffd4ede2199f6ea793f819e1abb019", new Genesis.Wallet("12345"),
                            "5bb088f57365907b1840e45984cae028a82af934", new Genesis.Wallet("0xabcdef"),
                            "0000a12b3f3d6c9b0d3f126a83ec2dd3dad15f39", new Genesis.Wallet("0x7fffffffffffffff")
                    )
            );
            ConcordModelSpecification spec = new ConcordModelSpecification(
                    "photon-3.0-64",
                    // "5b7eac22-976e-47fa-a000-1e09020a1c5d",
                    "8abc7fda-9576-4b13-9beb-06f867cf2c7c",
                    components
            );
            DeploymentSpecification deploySpec =
                    new DeploymentSpecification(clusterSize, spec, placementSpec, genesis);
            var request = new CreateClusterRequest(new MessageHeader(), deploySpec);
            // Check that the API can be serviced normally after service initialization.
            var promise = new CompletableFuture<DeploymentSessionIdentifier>();
            client.createCluster(request, Observers.newResultObserver(promise));
            var response = promise.get();
            log.info("response.getLow:" + response.getLow());
            log.info("response.getHigh:" + response.getHigh());
            return true;
        } catch (Throwable error) {
            log.error("Couldn't create client properly", error);
            return false;
        }
    }

    /**
     * CLI to create and delete cluster.
     * @param argv (provision-client server:port) OR
     *             (provision-client deleteCluster
     *             deploymentSessionId-low deploymentSessionId-high
     *             configJSON server:port)
     */
    public static void main(String[] argv) {
        // TODO:
        //  - use picocli
        //  - move createCluster to its own class with generic functionality
        if (argv[0].equals("deleteCluster")) {
            String config = null;
            try {
                config = Files.readString(Paths.get(argv[3]));
            } catch (IOException e) {
                log.info("Config passed is a string");
                config = argv[3];
            }

            DeploymentSessionIdentifier id = new DeploymentSessionIdentifier(Long.parseLong(argv[1]),
                    Long.parseLong(argv[2]));
            var cliObj = new ResourceDeprovisioner(id, config, argv[4]);
            try {
                cliObj.delete();
            } catch (RuntimeException e) {
                log.error("DeleteCluster CLI failed");
                e.printStackTrace();
            }
        } else {
            createFixedSizeCluster(argv[0]);
        }
    }
}
