/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.model.vsphere;

import java.util.List;

import lombok.Data;

/**
 * Data class.
 */
@Data
public class VirtualMachineGuestIdentityResponse {
    VirtualMachineGuestIdentityInfo value;

    /**
     * Data class.
     */
    @Data
    public static class VirtualMachineGuestIdentityInfo {
        String name;
        GuestOsFamily family;
        String hostName;
        LocalizableMessage fullName;
        String ipAddress;
    }

    /**
     * Data class.
     */
    @Data
    public static class LocalizableMessage {
        String id;
        String defaultMessage;
        List<String> args;
    }

    /**
     * Enum class.
     */
    enum GuestOsFamily {
        /** Windows operating system. */
        WINDOWS,
        /** Linux operating system. */
        LINUX,
        /** Novell Netware. */
        NETWARE,
        /** Solaris operating system. */
        SOLARIS,
        /** Mac OS operating system. */
        DARWIN,
        /** Other operating systems. */
        OTHER;
    }
}
