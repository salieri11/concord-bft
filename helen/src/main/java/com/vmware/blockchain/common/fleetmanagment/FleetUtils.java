/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.fleetmanagment;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import io.grpc.stub.StreamObserver;

/**
 * Some utilities to handle FleetManagment things.
 */
public class FleetUtils {
    private static final Logger logger = LogManager.getLogger(FleetUtils.class);

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
            logger.warn("Could not convert to identifier {} {}", c.getName(), uuid);
            return null;
        }
    }

    /**
     * Convert a persephone id into a UUID.
     * @param fleetId persephone ID
     * @return UUID
     */
    public static <T> UUID toUuid(T fleetId) {

        try {
            Method id = fleetId.getClass().getDeclaredMethod("getId");

            return UUID.fromString((String) id.invoke(fleetId));
        } catch (NoSuchMethodException | InvocationTargetException | IllegalAccessException e) {
            logger.warn("Could not convert to UUID {}", fleetId);
            return null;
        }
    }

    /**
     * An observer to fake a blocked call.
     */
    public static  <T> StreamObserver<T> blockedResultObserver(CompletableFuture<T> result) {
        return new StreamObserver<>() {
            /** Holder of result value. */
            volatile T value;

            @Override
            public void onNext(T value) {
                this.value = value;
            }

            @Override
            public void onError(Throwable error) {
                result.completeExceptionally(error);
            }

            @Override
            public void onCompleted() {
                result.complete(value);
            }
        };
    }


}
