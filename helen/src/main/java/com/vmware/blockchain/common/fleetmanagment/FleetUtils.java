/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.fleetmanagment;

import java.lang.reflect.InvocationTargetException;
import java.util.UUID;

/**
 * Some utilities to handle FleetManagment things.
 */
public class FleetUtils {

    /**
     * Convert a standard UUID into one of the FleetManagement identifiers.
     * This method assumes that all identifiers are of the form
     * new Identifier(long lowBits, long highBits)
     * @param c class of the type we are created
     * @param uuid  UUID we are converting
     * @return New instance of the identifier, or else null
     */
    public static <T> T identifier(Class<T> c, UUID uuid) {
        try {
            return c.getDeclaredConstructor(Long.TYPE, Long.TYPE)
                    .newInstance(uuid.getLeastSignificantBits(), uuid.getMostSignificantBits());
        } catch (NoSuchMethodException | InstantiationException
                | IllegalAccessException | InvocationTargetException e) {
            return null;
        }
    }



}
