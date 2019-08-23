/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.fleetmanagment;

import static com.vmware.blockchain.common.fleetmanagment.FleetUtils.identifier;
import static com.vmware.blockchain.common.fleetmanagment.FleetUtils.toUuid;

import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.deployment.v1.ConcordNodeIdentifier;

/**
 * Noce tests.
 */
@ExtendWith(SpringExtension.class)
public class FleetUtilsTest {

    @Test
    void testId() {
        UUID id = UUID.randomUUID();
        ConcordNodeIdentifier cni = identifier(ConcordNodeIdentifier.class, id);
        Assertions.assertNotNull(cni);
        Assertions.assertTrue(cni instanceof ConcordNodeIdentifier);
        Assertions.assertEquals(id.getLeastSignificantBits(), cni.getLow());
        Assertions.assertEquals(id.getMostSignificantBits(), cni.getHigh());

        UUID id2 = toUuid(cni);
        Assertions.assertEquals(id, id2);
    }

}
