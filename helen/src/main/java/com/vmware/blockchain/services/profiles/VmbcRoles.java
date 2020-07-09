/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import org.springframework.stereotype.Component;

import com.vmware.blockchain.base.auth.BaseRoles;
import com.vmware.blockchain.base.auth.Role;

/**
 * Class to define extra roles for Helen.
 */
@Component
public class VmbcRoles extends BaseRoles {

    // Extra Roles used by Helen.
    public static final Role ORG_DEVELOPER = addRole("vmbc-org:dev", "Organization Developer", false, false, false);
    public static final Role CONSORTIUM_ADMIN =
            addRole("vmbc-consortium:admin", "Consortium Admin", false, true, false);
    public static final Role
            CONSORTIUM_OPERATOR = addRole("vmbc-consortium:operator", "Consortium Operator", false, true, false);
    public static final Role CONSORTIUM_PARTICIPANT =
            addRole("vmbc-consortium:participant", "Consortium Participant", false, true, false);

    /**
     * Has a developer role.
     * @return developer roles.
     */
    public String[] developer() {
        String[] r = {INFRA_ADMIN.getName(), SYSTEM_ADMIN.getName(), CONSORTIUM_ADMIN.getName(), ORG_ADMIN.getName(),
                      ORG_DEVELOPER.getName()};
        return r;
    }

    public String[] consortiumAdmin() {
        String[] r = {INFRA_ADMIN.getName(), SYSTEM_ADMIN.getName(), CONSORTIUM_ADMIN.getName()};
        return r;
    }

    public String[] consortiumParticipant() {
        return new String[]{INFRA_ADMIN.getName(), SYSTEM_ADMIN.getName(), CONSORTIUM_ADMIN.getName(),
                ORG_ADMIN.getName(), CONSORTIUM_PARTICIPANT.getName()};
    }

}
