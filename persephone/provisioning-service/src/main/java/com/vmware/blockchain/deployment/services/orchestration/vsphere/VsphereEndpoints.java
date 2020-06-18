/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vsphere;


import lombok.Getter;

/**
 * Enumeration of URI endpoint with expected parameter names.
 */
public enum VsphereEndpoints {

    VSPHERE_AUTHENTICATION("/rest/com/vmware/cis/session"),
    VSPHERE_DATASTORES("/rest/vcenter/datastore"),
    VSPHERE_FOLDERS("/rest/vcenter/folder"),
    VSPHERE_CONTENT_LIBRARY_ITEM("/rest/com/vmware/content/library/item"),
    VSPHERE_RESOURCE_POOLS("/rest/vcenter/resource-pool"),
    VSPHERE_OVF_LIBRARY_ITEM("/rest/com/vmware/vcenter/ovf/library-item/id:{library_item}"),
    VSPHERE_NETWORKS("/rest/vcenter/network"),

    VSPHERE_VM("/rest/vcenter/vm/{vm}"),
    VSPHERE_VM_POWER("/rest/vcenter/vm/{vm}/power"),
    VSPHERE_VM_POWER_START("/rest/vcenter/vm/{vm}/power/start"),
    VSPHERE_VM_POWER_STOP("/rest/vcenter/vm/{vm}/power/stop"),
    VSPHERE_VM_CPU_UPDATE("/rest/vcenter/vm/{vm}/hardware/cpu"),
    VSPHERE_VM_MEMORY_UPDATE("/rest/vcenter/vm/{vm}/hardware/memory");

    @Getter
    private String path;

    VsphereEndpoints(String path) {
        this.path = path;
    }
}
