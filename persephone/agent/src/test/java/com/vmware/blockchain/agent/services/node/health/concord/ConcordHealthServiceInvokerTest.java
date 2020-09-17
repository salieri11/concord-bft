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

    private final String responsePositive = "{  \"Sequence Numbers \": {  "
            + "\"lastViewThatTransferredSeqNumbersFullyExecuted\": \"0\",  "
            + "\"mainLog->currentActiveWindow().first\": \"751\",  "
            + "\"maxSeqNumTransferredFromPrevViews\": \"0\",  "
            + "\"strictLowerBoundOfSeqNums\": \"750\",  "
            + "\"lastStableSeqNum\": \"" + validSeqNum + "\",  \"mainLog->currentActiveWindow().second\": \"1050\",  "
            + "\"primaryLastUsedSeqNum\": \"750\"},  \"Other \": {  \"recoveringFromExecutionOfRequests\": \"0\",  "
            + "\"timeOfLastStateSynch\": \"Fri Sep 18 23:38:38 2020 GMT\",  "
            + "\"lastTimeThisReplicaSentStatusReportMsgToAllPeerReplicas\": \"Fri Sep 18 23:40:38 2020 GMT\",  "
            + "\"batchingFactor\": \"1\",  \"clientsManager->numberOfRequiredReservedPages()\": \"464\",  "
            + "\"checkpointsLog->currentActiveWindow().second\": \"1199\",  "
            + "\"checkpointsLog->currentActiveWindow().first\": \"600\",  "
            + "\"maxNumberOfPendingRequestsInRecentHistory\": \"0\",  \"requestsQueueOfPrimary.size()\": \"0\",  "
            + "\"restarted_\": \"0\"},  \"View Change \": {  \"curView\": \"0\",  "
            + "\"timeOfLastAgreedView\": \"Fri Sep 18 23:38:38 2020 GMT\",  \"autoPrimaryRotationTimerMilli\": \"0\",  "
            + "\"viewChangeTimerMilli\": \"20000\",  \"autoPrimaryRotationEnabled\": \"0\",  "
            + "\"viewChangeProtocolEnabled\": \"1\",  \"timeOfLastViewEntrance\": \"Fri Sep 18 23:38:38 2020 GMT\",  "
            + "\"lastAgreedView\": \"0\"},  \"Replica ID\": 5,  \"Primary \": 0}";

    private final String responseNegative = "{  \"Sequence Numbers \": {  "
            + "\"lastViewThatTransferredSeqNumbersFullyExecuted\": \"0\",  "
            + "\"mainLog->currentActiveWindow().first\": \"751\",  "
            + "\"maxSeqNumTransferredFromPrevViews\": \"0\",  "
            + "\"strictLowerBoundOfSeqNums\": \"750\",  "
            + "\"lastStableSeqNum\": \"" + invalidSeqNum + "\",  \"mainLog->currentActiveWindow().second\": \"1050\",  "
            + "\"primaryLastUsedSeqNum\": \"750\"},  \"Other \": {  \"recoveringFromExecutionOfRequests\": \"0\",  "
            + "\"timeOfLastStateSynch\": \"Fri Sep 18 23:38:38 2020 GMT\",  "
            + "\"lastTimeThisReplicaSentStatusReportMsgToAllPeerReplicas\": \"Fri Sep 18 23:40:38 2020 GMT\",  "
            + "\"batchingFactor\": \"1\",  \"clientsManager->numberOfRequiredReservedPages()\": \"464\",  "
            + "\"checkpointsLog->currentActiveWindow().second\": \"1199\",  "
            + "\"checkpointsLog->currentActiveWindow().first\": \"600\",  "
            + "\"maxNumberOfPendingRequestsInRecentHistory\": \"0\",  \"requestsQueueOfPrimary.size()\": \"0\",  "
            + "\"restarted_\": \"0\"},  \"View Change \": {  \"curView\": \"0\",  "
            + "\"timeOfLastAgreedView\": \"Fri Sep 18 23:38:38 2020 GMT\",  \"autoPrimaryRotationTimerMilli\": \"0\",  "
            + "\"viewChangeTimerMilli\": \"20000\",  \"autoPrimaryRotationEnabled\": \"0\",  "
            + "\"viewChangeProtocolEnabled\": \"1\",  \"timeOfLastViewEntrance\": \"Fri Sep 18 23:38:38 2020 GMT\",  "
            + "\"lastAgreedView\": \"0\"},  \"Replica ID\": 5,  \"Primary \": 0}";

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
