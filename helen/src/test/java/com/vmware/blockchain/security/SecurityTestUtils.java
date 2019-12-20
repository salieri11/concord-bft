/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import java.util.Base64;
import java.util.Collections;
import java.util.UUID;

import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;

import com.vmware.blockchain.services.profiles.Consortium;
import com.vmware.blockchain.services.profiles.Organization;
import com.vmware.blockchain.services.profiles.User;
import com.vmware.blockchain.services.profiles.VmbcRoles;

/**
 * Just isolating some useful funtions shared by the security tests.
 */
public class SecurityTestUtils {
    // Just some random UUIDs
    public static final UUID USER_ID = UUID.fromString("f1c1aa4f-4958-4e93-8a51-930d595fb65b");
    public static final UUID ORG_ID = UUID.fromString("82634974-88cf-4944-a99d-6b92664bb765");
    public static final UUID CONSORTIUM_ID = UUID.fromString("5c7cd0e9-57ad-44af-902f-74af2f3dd8fe");
    public static final String SECRET_KEY = Base64.getEncoder().encodeToString("secret-key".getBytes());
    public static final UUID BC_ID = UUID.fromString("437d97b2-76df-4596-b0d8-3d8a9412ff2f");

    /**
     * Create a test User.
     */
    public static User getUser() {
        User user = User.builder()
                .email("user@test.com")
                .password(new BCryptPasswordEncoder(4).encode("1234"))
                .firstName("U")
                .lastName("Ser")
                .organization(ORG_ID)
                .serviceRoles(Collections.singletonList(VmbcRoles.ORG_USER))
                .build();
        user.setId(USER_ID);
        return user;
    }

    /**
     * Create a test Organization.
     */
    public static Organization getOrganization() {
        Organization org = new Organization("Test Org");
        org.setId(ORG_ID);
        return org;
    }

    /**
     * Create a test Consortium.
     */
    public static Consortium getConsortium() {
        Consortium c = new Consortium("Test Consortium", "Test", ORG_ID);
        c.setId(CONSORTIUM_ID);
        return c;
    }
}
