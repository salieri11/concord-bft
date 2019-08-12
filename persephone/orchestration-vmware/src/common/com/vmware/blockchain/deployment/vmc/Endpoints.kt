/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.vmc

import com.vmware.blockchain.deployment.http.EndpointEnumeration

/**
 * Enumeration of URI endpoint with expected parameter names.
 */
enum class Endpoints(
    override val path: String,
    override val parameterMappings: Set<String>,
    override val pathMappings: Set<String> = emptySet()
) : EndpointEnumeration {

    NSX_API_ROOT(
            "policy/api/v1/{resource}",
            emptySet(),
            setOf("{resource}")
    ),
    NSX_NAT_RULE(
            "policy/api/v1/infra/tier-1s/{tier1}/nat/{nat}/nat-rules/{nat_rule}",
            emptySet(),
            setOf("{tier1}", "{nat}", "{nat_rule}")
    ),
    NSX_NETWORK_SEGMENT(
            "policy/api/v1/infra/tier-1s/{tier1}/segments/{segment}",
            emptySet(),
            setOf("{tier1}", "{segment}")
    ),
    VMC_AUTHENTICATION("/csp/gateway/am/api/auth/api-tokens/authorize", setOf("refresh_token")),
    VMC_PUBLIC_IP(
            "cloud-service/api/v1/infra/public-ips/{ip_id}",
            emptySet(),
            setOf("{ip_id}")
    ),
    VMC_SDDC(
            "/vmc/api/orgs/{org}/sddcs/{sddc}",
            emptySet(),
            setOf("{org}", "{sddc}")
    );
}
