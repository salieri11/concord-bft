/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health.daml;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.agent.services.metrics.MetricsConstants;
import com.vmware.blockchain.agent.services.node.health.ComponentHealth;
import com.vmware.blockchain.agent.services.node.health.HealthStatusResponse;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.grpc.health.v1.HealthCheckRequest;
import com.vmware.blockchain.grpc.health.v1.HealthCheckResponse;
import com.vmware.blockchain.grpc.health.v1.HealthGrpc;

import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import io.micrometer.core.instrument.Tag;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

/**
 * daml health services invoker.
 */
@Slf4j
@Component
public class DamlHealthServiceInvoker {

    /**
     * Service enum.
     */
    public enum Service {
        COMMITTER(List.of("validator"), "daml_execution_engine", 55000),
        PARTICIPANT(List.of("index", "write"), "daml_ledger_api", 6865);

        List<String> serviceName;
        @Getter
        String host;
        int port;

        Service(List<String> serviceName, String host, int port) {
            this.serviceName = serviceName;
            this.host = host;
            this.port = port;
        }
    }

    private ManagedChannel channel;
    private HealthGrpc.HealthBlockingStub blockingStub;

    @Getter
    private String hostIp;

    @Getter
    private final Service service;

    @Getter
    private final ConcordModelSpecification.NodeType nodeType;

    @Autowired
    public DamlHealthServiceInvoker(ConcordModelSpecification.NodeType nodeType) {
        this.nodeType = nodeType;
        this.service = this.nodeType
                .equals(ConcordModelSpecification.NodeType.DAML_PARTICIPANT) ? Service.PARTICIPANT : Service.COMMITTER;
    }

    /**
     * Checks health of service.
     * @return list of {@link HealthCheckResponse}
     * @throws Exception exception.
     */
    public List<HealthCheckResponse> check() throws Exception {
        List<CompletableFuture<HealthCheckResponse>> actions = new ArrayList<>();
        getServices().forEach(serv -> {
            actions.add(CompletableFuture.supplyAsync(() -> this.getResponse(serv)));
        });

        return CompletableFuture.allOf(actions.toArray(new CompletableFuture[0]))
                .thenApply((res) -> actions.stream()
                .map(CompletableFuture::join)
                .collect(Collectors.toList())).get();

    }

    HealthCheckResponse getResponse(String serviceName) {
        HealthCheckRequest request = HealthCheckRequest.newBuilder().setService(serviceName).build();
        HealthCheckResponse response = blockingStub.check(request);
        log.info("Request - {}; Response - {}", request, response);
        return response;
    }

    /**
     * get health status of daml component.
     * @return {@link HealthStatusResponse}
     */
    public HealthStatusResponse getHealthResponse() {
        log.info("received health check request for daml components.");
        String metricsDesc = "Daml health status";
        List<Tag> tags = Collections.singletonList(
                Tag.of(MetricsConstants.MetricsTags.TAG_NODE_TYPE.name(), getNodeTypeName()));
        try {
            List<HealthCheckResponse.ServingStatus> statusList = this.check().stream()
                    .map(HealthCheckResponse::getStatus).collect(Collectors.toList());
            if (statusList.contains(HealthCheckResponse.ServingStatus.SERVING) && Collections.frequency(statusList,
                    HealthCheckResponse.ServingStatus.SERVING) == statusList.size()) {
                ComponentHealth.metricsAgent.gauge(1, metricsDesc,
                        MetricsConstants.MetricsNames.DAML_HEALTH_STATUS, tags);
                return HealthStatusResponse.builder().status(HealthStatusResponse.HealthStatus.HEALTHY).build();
            }
            ComponentHealth.metricsAgent.gauge(0, metricsDesc,
                    MetricsConstants.MetricsNames.DAML_HEALTH_STATUS, tags);
            return HealthStatusResponse.builder().status(HealthStatusResponse.HealthStatus.UNHEALTHY).build();
        } catch (Exception e) {
            ComponentHealth.metricsAgent.gauge(-1, metricsDesc,
                    MetricsConstants.MetricsNames.DAML_HEALTH_STATUS, tags);
            return HealthStatusResponse.builder()
                    .status(HealthStatusResponse.HealthStatus.SERVICE_UNAVAILABLE)
                    .exception(e.getLocalizedMessage()).build();
        }
    }

    /**
     * Placeholder to invoke idle/suspend condition.
     * @throws InterruptedException exception.
     */
    public void shutdown() throws InterruptedException {
        channel.shutdown().awaitTermination(5, TimeUnit.SECONDS);
    }

    /**
     * Placeholder to invoke channel.
     * NOTE: since "_" is not a valid literal in hostname, we can not use container name
     * Future work: If we have container names without "_", replace "host" with "service.host"
     */
    public void start(String host) {
        if (this.channel != null && host.equalsIgnoreCase(this.hostIp)) {
            log.info("gRPC channel for service {} on ip {} already created.", service.host, host);
            return;
        }

        this.hostIp = host;
        log.info("creating gRPC channel with host ip {} for {} on {}...", host, service.host, service.port);
        this.channel = ManagedChannelBuilder
                .forAddress(host, service.port)
                .usePlaintext()
                .build();

        this.blockingStub = HealthGrpc.newBlockingStub(channel);
        log.info("gRPC channel created.");
    }

    /**
     * get services: used to aid unit tests.
     * @return list of services.
     */
    List<String> getServices() {
        return this.service.serviceName;
    }

    /**
     * get node type name.
     * @return node type name.
     */
    String getNodeTypeName() {
        return this.nodeType.name();
    }
}
