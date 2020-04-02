/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vmc;

import lombok.Getter;

/**
 * Enumeration of URI endpoint with expected parameter names.
 */
enum VmcEndpoints {

    NSX_API_ROOT("/policy/api/v1/{resource}"),
    NSX_NAT_RULE("/policy/api/v1/infra/tier-1s/{tier1}/nat/{nat}/nat-rules/{nat_rule}"),
    NSX_NETWORK_SEGMENT("/policy/api/v1/infra/tier-1s/{tier1}/segments/{segment}"),
    VMC_AUTHENTICATION("/csp/gateway/am/api/auth/api-tokens/authorize"),
    VMC_PUBLIC_IP("/cloud-service/api/v1/infra/public-ips/{ip_id}"),
    VMC_SDDC("/vmc/api/orgs/{org}/sddcs/{sddc}");

    @Getter
    private String path;

    VmcEndpoints(String path) {
        this.path = path;
    }
}
