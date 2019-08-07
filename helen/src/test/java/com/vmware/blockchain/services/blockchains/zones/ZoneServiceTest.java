/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.zones;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;

import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.stubbing.Answer;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.common.fleetmanagment.FleetUtils;
import com.vmware.blockchain.deployment.model.ListOrchestrationSitesRequest;
import com.vmware.blockchain.deployment.model.ListOrchestrationSitesResponse;
import com.vmware.blockchain.deployment.model.MessageHeader;
import com.vmware.blockchain.deployment.model.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.model.OrchestrationSiteInfo.Type;
import com.vmware.blockchain.deployment.model.OrchestrationSiteServiceStub;
import com.vmware.blockchain.deployment.model.OrchestrationSiteView;

import io.grpc.stub.StreamObserver;

/**
 * Test the ZoneService.
 */
@ExtendWith(SpringExtension.class)
public class ZoneServiceTest {
    private static final UUID SITE_1 = UUID.fromString("84b9a0ed-c162-446a-b8c0-2e45755f3844");
    private static final UUID SITE_2 = UUID.fromString("275638a3-8860-4925-85de-c73d45cb7232");

    List<OrchestrationSiteView> siteList;

    @Mock
    OrchestrationSiteServiceStub client;

    private ZoneService zoneService;

    private void setCallbacks(Answer answer) {
        doAnswer(answer).when(client)
                .listOrchestrationSites(any(ListOrchestrationSitesRequest.class), any(StreamObserver.class));
    }

    @BeforeEach
    void setUp() {
        OrchestrationSiteView v1 =
                new OrchestrationSiteView(FleetUtils.identifier(OrchestrationSiteIdentifier.class, SITE_1),
                                                                Type.VMC, ImmutableMap.of("name", "US_WEST"));
        OrchestrationSiteView v2 =
                new OrchestrationSiteView(FleetUtils.identifier(OrchestrationSiteIdentifier.class, SITE_2),
                                          Type.NONE, ImmutableMap.of("name", "US_EAST"));
        ListOrchestrationSitesResponse response =
                new ListOrchestrationSitesResponse(new MessageHeader(),
                                                   ImmutableList.of(v1, v2),
                                                   "");
        setCallbacks(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(response);
            ob.onCompleted();
            return null;
        });

        zoneService = new ZoneService(client);
    }

    @Test
    void testLoad() throws Exception {
        zoneService.loadZones();
        List<Zone> l = zoneService.getZones();
        Assertions.assertEquals(2, l.size());
        Zone z1 = l.get(0);
        Zone z2 = l.get(1);
        Assertions.assertEquals(SITE_1, z1.getId());
        Assertions.assertEquals(Type.VMC, z1.getType());
        Assertions.assertEquals("US_WEST", z1.getLabels().get("name"));
        Assertions.assertEquals(SITE_2, z2.getId());
        Assertions.assertEquals(Type.NONE, z2.getType());
        Assertions.assertEquals("US_EAST", z2.getLabels().get("name"));
    }
}