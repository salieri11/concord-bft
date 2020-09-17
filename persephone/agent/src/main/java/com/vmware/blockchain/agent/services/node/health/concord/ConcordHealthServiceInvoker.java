/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health.concord;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.agent.services.metrics.MetricsAgent;
import com.vmware.blockchain.agent.services.metrics.MetricsConstants;
import com.vmware.blockchain.agent.services.node.health.ComponentHealth;
import com.vmware.blockchain.agent.services.node.health.HealthStatusResponse;

import io.micrometer.core.instrument.Tag;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

/**
 * class to invoke concord health services.
 */
@Component
@Slf4j
public class ConcordHealthServiceInvoker {

    private final int concordPort;

    @Getter
    private Socket socket;

    @Getter
    private volatile Long stableSeqNum;

    @Getter
    private final String container = "concord";
    private final MetricsAgent metricsAgent;

    private AtomicBoolean disposed;
    private volatile String host;

    @Autowired
    ConcordHealthServiceInvoker(MetricsAgent metricsAgent) {
        this.concordPort = 6888;
        this.stableSeqNum = 0L;
        this.metricsAgent = metricsAgent;
    }

    /**
     * populates host ip of container.
     * @param host ip of container.
     */
    public synchronized void registerHost(String host) {
        this.host = host;
    }

    /**
     * Creates socket.
     */
    void createSocket() {

        disposed = new AtomicBoolean(false);
        try {
            if (this.socket != null && !this.socket.isClosed()) {
                String msg = "Socket already exists and open";
                log.error(msg);
                throw new IOException(msg);
            }
            this.socket = new Socket(this.host, this.concordPort);
        } catch (IOException ex) {
            log.error("Error creating TCP connection with Concord. Host={}, port={}. \nException: {}",
                    host, this.concordPort, ex.getLocalizedMessage());
            log.error("********************** Concord health check will not be unavailable ************************\n");
            logMetrics(-1);
        }
    }

    /**
     * gets health status for concord.
     * @return {@link HealthStatusResponse}
     */
    public synchronized HealthStatusResponse getConcordHealth() {
        try {
            String cmd = "status get replica\n";
            log.info("Sending request to concord socker server");
            // NOTE: there is a timeout of 1second on concord server code. Time taken by the log flushes and
            // variable instantiations are causing to get past the timeout. To be safe, logging everything later.
            createSocket();
            OutputStreamWriter outputStreamWriter = new OutputStreamWriter(this.socket.getOutputStream(),
                    StandardCharsets.UTF_8);
            BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(this.socket.getInputStream()));
            outputStreamWriter.write(cmd, 0, cmd.length());
            outputStreamWriter.flush();
            String response = bufferedReader.lines().collect(Collectors.joining());
            log.info("Message received from concord:\n{}", response);
            bufferedReader.close();
            outputStreamWriter.close();
            return getHealthStatus(response);
        } catch (Exception ex) {
            log.error("Exception in socket response:\n{}", ex);
            logMetrics(-1);
            return HealthStatusResponse.builder()
                    .status(HealthStatusResponse.HealthStatus.SERVICE_UNAVAILABLE)
                    .exception(ex.getLocalizedMessage())
                    .build();
        } finally {
            log.info("Client socket host: {}, port: {}", this.socket.getLocalAddress(), this.socket.getLocalPort());
            closeSocket();
        }
    }

    /**
     * gets health status.
     * @param response socket response string
     * @return {@link HealthStatusResponse}
     */
    HealthStatusResponse getHealthStatus(String response) {
        try {
            HealthStatusResponse healthStatusResponse = null;
            JSONParser parser = new JSONParser();
            JSONObject json = (JSONObject) parser.parse(response);

            for (Object obj : json.entrySet()) {
                Map.Entry<String, Object> entry = (Map.Entry<String, Object>) obj;
                if (entry.getKey().equalsIgnoreCase("Sequence Numbers ")) {
                    Map<String, String> value = (Map<String, String>) entry.getValue();
                    var seqNum = Long.parseLong(value.get("lastStableSeqNum"));
                    log.info("lastStableSeqNum: {}", seqNum);
                    if (seqNum <= this.stableSeqNum) {
                        healthStatusResponse = HealthStatusResponse.builder()
                                .status(HealthStatusResponse.HealthStatus.UNHEALTHY)
                                .build();
                        logMetrics(0);
                    } else {
                        this.stableSeqNum = seqNum;
                        healthStatusResponse = HealthStatusResponse.builder()
                                .status(HealthStatusResponse.HealthStatus.HEALTHY)
                                .build();
                        logMetrics(1);
                    }
                    break;
                }
            }
            return healthStatusResponse;
        } catch (Exception ex) {
            log.error("JSON Parse exception from concord: {}", ex.getLocalizedMessage());
            logMetrics(-1);
            return HealthStatusResponse.builder()
                    .status(HealthStatusResponse.HealthStatus.SERVICE_UNAVAILABLE)
                    .exception(ex.getLocalizedMessage())
                    .build();
        }
    }

    /**
     * logs metrics.
     * @param value value to gauge
     */
    void logMetrics(int value) {
        List<Tag> tags = Arrays.asList(ComponentHealth.tag,
                Tag.of(MetricsConstants.MetricsTags.TAG_NODE_TYPE.metricsTagName, "concord"));
        int val = this.metricsAgent.gaugeValue(value, MetricsConstants.MetricsNames.CONCORD_HEALTH_STATUS, tags);
        log.info("Metric {} is updated with value {}", MetricsConstants.MetricsNames.CONCORD_HEALTH_STATUS.metricsName,
                val);
    }

    /**
     * close concord socket.
     */
    public void closeSocket() {
        if (disposed.get()) {
            return;
        }

        if (this.socket != null && !this.socket.isClosed()) {
            try {
                this.socket.close();
            } catch (IOException e) {
                log.error("Error in closing TCP socket");
            } finally {
                disposed.set(true);
            }
        }
    }
}
