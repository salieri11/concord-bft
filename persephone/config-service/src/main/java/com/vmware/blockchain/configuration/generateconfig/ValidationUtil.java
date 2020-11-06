/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

/**
 * Utility class for vaidation methods.
 */
public class ValidationUtil {
    /**
     * Validate if the object is non null.
     * @param obj Input object
     * @return True if the object is not null, false otherwise.
     */
    public static boolean isValid(Object obj) {
        if (obj == null) {
            return false;
        }
        return true;
    }
}
