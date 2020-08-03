/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.util.Objects;
import java.util.Optional;

import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;

/**
 * Utility methods.
 */
public class DescriptorUtils {

    /**
     * Given a zone name, get the zone details from the infra descriptor.
     * @param zoneName the zone name
     * @param infrastructureDescriptorModel the infra model from which to read the zone info
     * @return the zone info wrapped in an Optional
     */
    public static Optional<InfrastructureDescriptorModel.Zone> getZone(
            String zoneName, InfrastructureDescriptorModel infrastructureDescriptorModel) {
        Optional<InfrastructureDescriptorModel.Zone> zoneInfoOpt =
                infrastructureDescriptorModel.getZones().stream()
                        .filter(z -> Objects.equals(zoneName, z.getName()))
                        .findFirst();
        return zoneInfoOpt;
    }
}
