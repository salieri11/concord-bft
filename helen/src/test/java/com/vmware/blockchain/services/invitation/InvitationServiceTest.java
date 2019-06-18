/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.invitation;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.google.common.collect.ImmutableList;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.csp.CspCommon;
import com.vmware.blockchain.common.csp.CspCommon.CspPatchServiceRolesRequest;
import com.vmware.blockchain.common.csp.CspCommon.CspServiceInvitation;
import com.vmware.blockchain.common.csp.CspConstants;
import com.vmware.blockchain.common.csp.api.client.CspApiClient;
import com.vmware.blockchain.services.profiles.Roles;

/**
 * Test the InvitationService.
 */
@ExtendWith(SpringExtension.class)
class InvitationServiceTest {

    @Mock
    CspApiClient cspApiClient;

    @Mock
    AuthHelper authHelper;

    @Captor
    ArgumentCaptor<String> tokenCap;

    @Captor
    ArgumentCaptor<UUID> orgCap;

    @Captor
    ArgumentCaptor<String> emailCap;

    @Captor
    ArgumentCaptor<CspCommon.CspPatchServiceRolesRequest> roleCap;


    private String serviceDefLink =
            CspConstants.CSP_SERVICE_DEFINTION + "/external/57df0bf1-69fd-4770-9313-ffab602d9f00";
    private String orgLink = CspConstants.CSP_ORG_API + "/31ff9608-dbf7-4daf-a13b-82ba443e7721";
    private UUID orgId = UUID.fromString("31ff9608-dbf7-4daf-a13b-82ba443e7721");
    private String invitationLink = "csp/inviation/20bc66aa-9332-499b-9697-904577257945";

    private InvitationService invitationService;

    @BeforeEach
    void init() throws Exception {
        invitationService = new InvitationService(cspApiClient, authHelper, "57df0bf1-69fd-4770-9313-ffab602d9f00");
        CspCommon.CspServiceInvitation invitation = new CspServiceInvitation();
        invitation.setServiceDefinitionLink(serviceDefLink);
        invitation.setOrgLink(orgLink);
        invitation.setInvitationLink(invitationLink);
        doThrow(RuntimeException.class).when(cspApiClient).getInvitation("argle");
        when(cspApiClient.getInvitation(invitationLink)).thenReturn(invitation);
        when(authHelper.getOrganizationId()).thenReturn(orgId);
        when(authHelper.getEmail()).thenReturn("test@email.com");
        when(authHelper.getAuthToken()).thenReturn("atoken");
    }

    @Test
    void testGoodRetrieve() throws Exception {
        invitationService.handleServiceInvitation(invitationLink);
        verify(cspApiClient, times(1)).patchOrgServiceRoles(anyString(), any(UUID.class), anyString(),
                                                            any(CspPatchServiceRolesRequest.class));
        verify(cspApiClient).patchOrgServiceRoles(tokenCap.capture(), orgCap.capture(),
                                                  emailCap.capture(), roleCap.capture());
        Assertions.assertEquals("atoken", tokenCap.getValue());
        Assertions.assertEquals(orgId, orgCap.getValue());
        Assertions.assertEquals("test@email.com", emailCap.getValue());
        List<String> expectedRoles = ImmutableList.of(Roles.CONSORTIUM_ADMIN.toString(), Roles.ORG_ADMIN.toString());
        List<String> actualRoles = roleCap.getValue().getRoleNamesToAdd();
        Assertions.assertEquals(expectedRoles, actualRoles);
    }

    @Test
    void testBadInvite() throws Exception {
        invitationService.handleServiceInvitation("argle");
        verify(cspApiClient, times(0)).patchOrgServiceRoles(anyString(), any(UUID.class), anyString(),
                                                            any(CspPatchServiceRolesRequest.class));
    }

    @Test
    void testBadOrg() throws Exception {
        when(authHelper.getOrganizationId()).thenReturn(UUID.fromString("5e93e41c-3d1c-43ac-bec7-a8c13670e72e"));
        invitationService.handleServiceInvitation(invitationLink);
        verify(cspApiClient, times(0)).patchOrgServiceRoles(anyString(), any(UUID.class), anyString(),
                                                            any(CspPatchServiceRolesRequest.class));
    }



}