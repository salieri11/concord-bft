/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health.concord;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
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

    @Autowired
    ConcordHealthServiceInvoker(MetricsAgent metricsAgent) {
        this.concordPort = 6888;
        this.stableSeqNum = 0L;
        this.metricsAgent = metricsAgent;
    }

    /**
     * Creates socket.
     * @param host string host
     */
    public synchronized void createSocket(String host) {

        disposed = new AtomicBoolean(false);
        try {
            this.socket = new Socket(host, this.concordPort);
            log.info("Connected to host: {} :: port: {}", host, this.concordPort);
        } catch (IOException ex) {
            log.error("Error creating TCP connection with Concord. Host={}, port={}. \nException: {}",
                    host, this.concordPort, ex.getLocalizedMessage());
            log.error("********************** Concord health check will not be unavailable ************************\n");
        }
    }

    /**
     * gets health status for concord.
     * @return {@link HealthStatusResponse}
     */
    public synchronized HealthStatusResponse getConcordHealth() {
        try {
            ObjectOutputStream outputStream = new ObjectOutputStream(this.socket.getOutputStream());
            log.info("Sending request to concord socker server");
            outputStream.writeObject("status get replica");
            ObjectInputStream inputStream = new ObjectInputStream(this.socket.getInputStream());
            String response = (String) inputStream.readObject();
            log.info("Message received from concord: {}", response);
            inputStream.close();
            outputStream.close();

            return getHealthStatus(response);
        } catch (Exception ex) {
            log.error("Exception in socket response: {}", ex.getLocalizedMessage());
            logMetrics(-1);
            return HealthStatusResponse.builder()
                    .status(HealthStatusResponse.HealthStatus.SERVICE_UNAVAILABLE)
                    .exception(ex.getLocalizedMessage())
                    .build();
        }
    }

    HealthStatusResponse getHealthStatus(String response) {
        try {
            HealthStatusResponse healthStatusResponse = null;
            JSONParser parser = new JSONParser();
            JSONObject json = (JSONObject) parser.parse(response);

            for (Object obj : json.entrySet()) {
                Map.Entry<String, Object> entry = (Map.Entry<String, Object>) obj;
                if (entry.getKey().equalsIgnoreCase("Sequence Numbers ")) {
                    Map<String, Long> value = (Map<String, Long>) entry.getValue();
                    var seqNum = value.get("lastStableSeqNum");
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
        } catch (ParseException ex) {
            log.error("JSON Parse exception from concord: {}", ex.getLocalizedMessage());
            logMetrics(-1);
            return HealthStatusResponse.builder()
                    .status(HealthStatusResponse.HealthStatus.SERVICE_UNAVAILABLE)
                    .exception(ex.getLocalizedMessage())
                    .build();
        }
    }

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
    public synchronized void closeSocket() {
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
