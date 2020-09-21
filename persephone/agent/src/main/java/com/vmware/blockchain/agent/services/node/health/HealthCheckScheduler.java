/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health;

import java.util.Calendar;
import java.util.Date;
import java.util.concurrent.ScheduledFuture;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

/**
 * health check scheduler.
 */
@Component
@Slf4j
public class HealthCheckScheduler {
    @Autowired
    private NodeComponentHealthFactory nodeComponentHealthFactory;

    @Autowired
    private ConcordModelSpecification.NodeType nodeType;

    @Autowired
    private TaskScheduler taskScheduler;

    @Getter
    private ScheduledFuture<?> scheduledFuture;

    void runHealthCheck() {
        ConcordComponent.ServiceType concordServiceType = null;
        ConcordComponent.ServiceType damlServiceType = null;
        switch (nodeType) {
            case DAML_COMMITTER:
                damlServiceType = ConcordComponent.ServiceType.DAML_EXECUTION_ENGINE;
                concordServiceType = ConcordComponent.ServiceType.CONCORD;
                break;
            case DAML_PARTICIPANT:
                damlServiceType = ConcordComponent.ServiceType.DAML_LEDGER_API;
                break;
            default:
                concordServiceType = ConcordComponent.ServiceType.CONCORD;
                break;
        }
        if (concordServiceType != null) {
            log.info("Calling concord health check in background..");
            nodeComponentHealthFactory.getHealthComponent(concordServiceType).getHealth();
        }
        if (damlServiceType != null) {
            log.info("Calling daml health check in background..");
            nodeComponentHealthFactory.getHealthComponent(damlServiceType).getHealth();
        }
    }

    /**
     * start periodic component health check.
     * @throws IllegalStateException when already running.
     */
    public synchronized void startHealthCheck() throws IllegalStateException {
        if (scheduledFuture == null || scheduledFuture.isDone()) {
            log.info("Scheduling periodic healthcheck with interval of 30 seconds.");
            scheduledFuture = taskScheduler.scheduleAtFixedRate(this::runHealthCheck,
                    addSecondsToCurrentDate(new Date(), 60), 30000);
        } else {
            var msg = "Periodic healthcheck is already running.";
            log.info(msg);
            throw new IllegalStateException(msg);
        }

    }

    /**
     * Stop periodic component health check.
     * @throws IllegalStateException when already stopped.
     */
    public synchronized void stopHealthCheck() throws IllegalStateException {
        if (scheduledFuture == null || scheduledFuture.isCancelled()) {
            var msg = "Periodic health check is not running, nothing to cancel.";
            log.info(msg);
            throw new IllegalStateException(msg);
        }
        log.info("Stoping periodic healthcheck...");
        scheduledFuture.cancel(false);
    }

    Date addSecondsToCurrentDate(Date date, int seconds) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        calendar.add(Calendar.SECOND, seconds);
        return calendar.getTime();
    }
}
