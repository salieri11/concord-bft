/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health.concord;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.agent.services.metrics.MetricsAgent;
import com.vmware.blockchain.agent.services.node.health.HealthStatusResponse;

import io.micrometer.core.instrument.simple.SimpleMeterRegistry;

/**
 * Test for {@link ConcordHealthServiceInvoker}.
 */
@ExtendWith(SpringExtension.class)
@ComponentScan(basePackageClasses = { ConcordHealthServiceInvoker.class})
public class ConcordHealthServiceInvokerTest {

    private final MetricsAgent metricsAgent = new MetricsAgent(new SimpleMeterRegistry());

    private final ConcordHealthServiceInvoker concordHealthServiceInvoker =
            new ConcordHealthServiceInvoker(metricsAgent);

    private final Long validSeqNum = 150L;
    private final Long invalidSeqNum = 0L;

    private final String responsePositive = "{\n"
            + "  \"Sequence Numbers \": {\n"
            + "  \"lastViewThatTransferredSeqNumbersFullyExecuted\": \"1\",\n"
            + "  \"mainLog->currentActiveWindow().first\": \"151\",\n"
            + "  \"maxSeqNumTransferredFromPrevViews\": \"0\",\n"
            + "  \"strictLowerBoundOfSeqNums\": \"150\",\n"
            + "  \"lastStableSeqNum\": " + validSeqNum + ",\n"
            + "  \"mainLog->currentActiveWindow().second\": \"450\",\n"
            + "  \"primaryLastUsedSeqNum\": \"150\"\n"
            + "},\n"
            + "  \"Other \": {\n"
            + "  \"recoveringFromExecutionOfRequests\": \"0\",\n"
            + "  \"timeOfLastStateSynch\": \"Tue Sep  8 18:29:06 2020 GMT\",\n"
            + "  \"lastTimeThisReplicaSentStatusReportMsgToAllPeerReplicas\": \"Tue Sep  8 18:30:45 2020 GMT\",\n"
            + "  \"batchingFactor\": \"1\",\n"
            + "  \"clientsManager->numberOfRequiredReservedPages()\": \"464\",\n"
            + "  \"checkpointsLog->currentActiveWindow().second\": \"599\",\n"
            + "  \"checkpointsLog->currentActiveWindow().first\": \"0\",\n"
            + "  \"maxNumberOfPendingRequestsInRecentHistory\": \"0\",\n"
            + "  \"requestsQueueOfPrimary.size()\": \"0\",\n"
            + "  \"restarted_\": \"0\"\n"
            + "},\n"
            + "  \"View Change \": {\n"
            + "  \"curView\": \"1\",\n"
            + "  \"timeOfLastAgreedView\": \"Tue Sep  8 18:29:46 2020 GMT\",\n"
            + "  \"autoPrimaryRotationTimerMilli\": \"0\",\n"
            + "  \"viewChangeTimerMilli\": \"20000\",\n"
            + "  \"autoPrimaryRotationEnabled\": \"0\",\n"
            + "  \"viewChangeProtocolEnabled\": \"1\",\n"
            + "  \"timeOfLastViewEntrance\": \"Tue Sep  8 18:29:46 2020 GMT\",\n"
            + "  \"lastAgreedView\": \"1\"\n"
            + "},\n"
            + "  \"Replica ID\": 2,\n"
            + "  \"Primary \": 1\n"
            + "}";

    private final String responseNegative = "{\n"
            + "  \"Sequence Numbers \": {\n"
            + "  \"lastViewThatTransferredSeqNumbersFullyExecuted\": \"1\",\n"
            + "  \"mainLog->currentActiveWindow().first\": \"151\",\n"
            + "  \"maxSeqNumTransferredFromPrevViews\": \"0\",\n"
            + "  \"strictLowerBoundOfSeqNums\": \"150\",\n"
            + "  \"lastStableSeqNum\": " + invalidSeqNum + ",\n"
            + "  \"mainLog->currentActiveWindow().second\": \"450\",\n"
            + "  \"primaryLastUsedSeqNum\": \"150\"\n"
            + "},\n"
            + "  \"Other \": {\n"
            + "  \"recoveringFromExecutionOfRequests\": \"0\",\n"
            + "  \"timeOfLastStateSynch\": \"Tue Sep  8 18:29:06 2020 GMT\",\n"
            + "  \"lastTimeThisReplicaSentStatusReportMsgToAllPeerReplicas\": \"Tue Sep  8 18:30:45 2020 GMT\",\n"
            + "  \"batchingFactor\": \"1\",\n"
            + "  \"clientsManager->numberOfRequiredReservedPages()\": \"464\",\n"
            + "  \"checkpointsLog->currentActiveWindow().second\": \"599\",\n"
            + "  \"checkpointsLog->currentActiveWindow().first\": \"0\",\n"
            + "  \"maxNumberOfPendingRequestsInRecentHistory\": \"0\",\n"
            + "  \"requestsQueueOfPrimary.size()\": \"0\",\n"
            + "  \"restarted_\": \"0\"\n"
            + "},\n"
            + "  \"View Change \": {\n"
            + "  \"curView\": \"1\",\n"
            + "  \"timeOfLastAgreedView\": \"Tue Sep  8 18:29:46 2020 GMT\",\n"
            + "  \"autoPrimaryRotationTimerMilli\": \"0\",\n"
            + "  \"viewChangeTimerMilli\": \"20000\",\n"
            + "  \"autoPrimaryRotationEnabled\": \"0\",\n"
            + "  \"viewChangeProtocolEnabled\": \"1\",\n"
            + "  \"timeOfLastViewEntrance\": \"Tue Sep  8 18:29:46 2020 GMT\",\n"
            + "  \"lastAgreedView\": \"1\"\n"
            + "},\n"
            + "  \"Replica ID\": 2,\n"
            + "  \"Primary \": 1\n"
            + "}";

    private final HealthStatusResponse healthy = HealthStatusResponse.builder()
            .status(HealthStatusResponse.HealthStatus.HEALTHY)
            .build();
    private final HealthStatusResponse unhealthy = HealthStatusResponse.builder()
            .status(HealthStatusResponse.HealthStatus.UNHEALTHY)
            .build();

    @Test
    void getHealthStatusTestPositive() {
        HealthStatusResponse actual = concordHealthServiceInvoker.getHealthStatus(responsePositive);
        Assertions.assertEquals(healthy, actual);
        Assertions.assertEquals(concordHealthServiceInvoker.getStableSeqNum(), validSeqNum);
    }

    @Test
    void getHealthStatusTestNegative() {
        HealthStatusResponse actual = concordHealthServiceInvoker.getHealthStatus(responseNegative);
        Assertions.assertEquals(unhealthy, actual);
    }

    @Test
    void testIfLastStableNumIsUpdatedOnlyOnPositive() {
        concordHealthServiceInvoker.getHealthStatus(responsePositive);
        Assertions.assertEquals(concordHealthServiceInvoker.getStableSeqNum(), validSeqNum);
        concordHealthServiceInvoker.getHealthStatus(responseNegative);
        Assertions.assertNotEquals(concordHealthServiceInvoker.getStableSeqNum(), invalidSeqNum);
        Assertions.assertEquals(concordHealthServiceInvoker.getStableSeqNum(), validSeqNum);
    }
}
