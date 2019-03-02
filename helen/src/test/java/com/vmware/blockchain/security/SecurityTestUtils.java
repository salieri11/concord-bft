/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Base64;
import java.util.UUID;

import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;

import com.vmware.blockchain.services.profiles.Consortium;
import com.vmware.blockchain.services.profiles.Organization;
import com.vmware.blockchain.services.profiles.Roles;
import com.vmware.blockchain.services.profiles.User;

/**
 * Just isolating some useful funtions shared by the security tests.
 */
public class SecurityTestUtils {
    // Just some random UUIDs
    public static final UUID USER_ID = UUID.fromString("f1c1aa4f-4958-4e93-8a51-930d595fb65b");
    public static final UUID ORG_ID = UUID.fromString("82634974-88cf-4944-a99d-6b92664bb765");
    public static final UUID CONSORTIUM_ID = UUID.fromString("5c7cd0e9-57ad-44af-902f-74af2f3dd8fe");
    public static final String SECRET_KEY = Base64.getEncoder().encodeToString("secret-key".getBytes());

    /**
     * Create a user mock with default values.
     */
    public static User createMockUser() {
        User user = mock(User.class);
        Organization organization = mock(Organization.class);
        Consortium consortium = mock(Consortium.class);
        when(consortium.getConsortiumId()).thenReturn(CONSORTIUM_ID);
        when(consortium.getConsortiumName()).thenReturn("Consortium Test");
        when(consortium.getConsortiumType()).thenReturn("Constorium Type");
        when(organization.getOrganizationId()).thenReturn(ORG_ID);
        when(organization.getOrganizationName()).thenReturn("Test Org");
        when(user.getUserId()).thenReturn(USER_ID);
        when(user.getEmail()).thenReturn("user@test.com");
        when(user.getPassword()).thenReturn(new BCryptPasswordEncoder(4).encode("1234"));
        when(user.getFirstName()).thenReturn("U");
        when(user.getLastName()).thenReturn("Ser");
        when(user.getConsortium()).thenReturn(consortium);
        when(user.getOrganization()).thenReturn(organization);
        when(user.getRole()).thenReturn(Roles.ORG_USER.toString());
        when(user.getRoles()).thenReturn(Arrays.asList(Roles.ORG_USER));
        return user;
    }
}
