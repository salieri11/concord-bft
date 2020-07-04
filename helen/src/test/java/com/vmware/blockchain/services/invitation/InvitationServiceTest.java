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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.google.common.collect.ImmutableList;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.csp.CspCommon;
import com.vmware.blockchain.common.csp.CspCommon.CspPatchServiceRolesRequest;
import com.vmware.blockchain.common.csp.CspCommon.CspServiceInvitation;
import com.vmware.blockchain.common.csp.CspConstants;
import com.vmware.blockchain.common.csp.api.client.CspApiClient;
import com.vmware.blockchain.common.csp.exception.CspApiClientErrorException;
import com.vmware.blockchain.services.profiles.Organization;
import com.vmware.blockchain.services.profiles.OrganizationService;
import com.vmware.blockchain.services.profiles.VmbcRoles;

/**
 * Test the InvitationService.
 */
@ExtendWith(SpringExtension.class)
class InvitationServiceTest {

    @Mock
    CspApiClient cspApiClient;

    @MockBean
    OrganizationService organizationService;

    @Mock
    AuthHelper authHelper;

    @Captor
    ArgumentCaptor<String> tokenCap;

    @Captor
    ArgumentCaptor<UUID> orgIdCap;

    @Captor
    ArgumentCaptor<Organization> orgCap;

    @Captor
    ArgumentCaptor<String> emailCap;

    @Captor
    ArgumentCaptor<CspCommon.CspPatchServiceRolesRequest> roleCap;


    private String serviceDefLink =
            CspConstants.CSP_SERVICE_DEFINITION + "/external/57df0bf1-69fd-4770-9313-ffab602d9f00";
    private String orgLink = CspConstants.CSP_ORG_API + "/31ff9608-dbf7-4daf-a13b-82ba443e7721";
    private UUID orgId = UUID.fromString("31ff9608-dbf7-4daf-a13b-82ba443e7721");
    private String invitationLink = "csp/inviation/20bc66aa-9332-499b-9697-904577257945";

    private InvitationService invitationService;
    private CspServiceInvitation invitation;

    @BeforeEach
    void init() throws Exception {
        invitationService = new InvitationService(cspApiClient, authHelper, organizationService,
                "57df0bf1-69fd-4770-9313-ffab602d9f00");
        invitation = new CspServiceInvitation();
        invitation.setServiceDefinitionLink(serviceDefLink);
        invitation.setOrgLink(orgLink);
        invitation.setInvitationLink(invitationLink);
        doThrow(new CspApiClientErrorException(HttpStatus.BAD_REQUEST, "", new HttpHeaders()))
                .when(cspApiClient).getInvitation("argle");
        CspCommon.CspUser user = new CspCommon.CspUser();
        user.setUsername("test@email.com");
        when(cspApiClient.getUser(anyString())).thenReturn(user);
        when(cspApiClient.getInvitation(invitationLink)).thenReturn(invitation);
        when(authHelper.getOrganizationId()).thenReturn(orgId);
        when(authHelper.getEmail()).thenReturn("test@email.com");
        when(authHelper.getAuthToken()).thenReturn("atoken");
        Organization org1 = new Organization("Org 1");
        org1.setId(orgId);
        when(organizationService.get(orgId)).thenReturn(org1);
    }

    @Test
    void testGoodRetrieve() throws Exception {
        invitationService.handleServiceInvitation(invitationLink);
        verify(cspApiClient, times(1)).patchOrgServiceRoles(anyString(), any(UUID.class), anyString(),
                                                            any(CspPatchServiceRolesRequest.class));
        verify(cspApiClient).patchOrgServiceRoles(tokenCap.capture(), orgIdCap.capture(),
                                                  emailCap.capture(), roleCap.capture());
        Assertions.assertEquals("atoken", tokenCap.getValue());
        Assertions.assertEquals(orgId, orgIdCap.getValue());
        Assertions.assertEquals("test@email.com", emailCap.getValue());
        List<String> expectedRoles =
                ImmutableList.of(VmbcRoles.CONSORTIUM_ADMIN.toString(), VmbcRoles.ORG_ADMIN.toString());
        List<String> actualRoles = roleCap.getValue().getRoleNamesToAdd();
        Assertions.assertEquals(expectedRoles, actualRoles);
    }

    @Test
    void testAdditionalRoles() throws Exception {
        HashMap<String, String> context = new HashMap<>();
        context.put(Constants.INVITATION_ROLE, VmbcRoles.INFRA_ADMIN.toString());
        invitation.setContext(context);
        invitationService.handleServiceInvitation(invitationLink);
        verify(cspApiClient, times(1)).patchOrgServiceRoles(anyString(), any(UUID.class), anyString(),
                                                            any(CspPatchServiceRolesRequest.class));
        verify(cspApiClient).patchOrgServiceRoles(tokenCap.capture(), orgIdCap.capture(),
                                                  emailCap.capture(), roleCap.capture());
        Assertions.assertEquals("atoken", tokenCap.getValue());
        Assertions.assertEquals(orgId, orgIdCap.getValue());
        Assertions.assertEquals("test@email.com", emailCap.getValue());
        List<String> expectedRoles =
                ImmutableList.of(VmbcRoles.INFRA_ADMIN.toString(), VmbcRoles.CONSORTIUM_ADMIN.toString(),
                                 VmbcRoles.ORG_ADMIN.toString());
        List<String> actualRoles = roleCap.getValue().getRoleNamesToAdd();
        Assertions.assertEquals(expectedRoles, actualRoles);
    }

    @Test
    void testMultipleAdditionalRoles() throws Exception {
        HashMap<String, String> context = new HashMap<>();
        context.put(Constants.INVITATION_ROLE, "vmbc-system:admin, vmbc-system:infra");
        invitation.setContext(context);
        invitationService.handleServiceInvitation(invitationLink);
        verify(cspApiClient, times(1)).patchOrgServiceRoles(anyString(), any(UUID.class), anyString(),
                                                            any(CspPatchServiceRolesRequest.class));
        verify(cspApiClient).patchOrgServiceRoles(tokenCap.capture(), orgIdCap.capture(),
                                                  emailCap.capture(), roleCap.capture());
        Assertions.assertEquals("atoken", tokenCap.getValue());
        Assertions.assertEquals(orgId, orgIdCap.getValue());
        Assertions.assertEquals("test@email.com", emailCap.getValue());
        List<String> expectedRoles =
                ImmutableList.of(VmbcRoles.SYSTEM_ADMIN.toString(), VmbcRoles.INFRA_ADMIN.toString(),
                                 VmbcRoles.CONSORTIUM_ADMIN.toString(), VmbcRoles.ORG_ADMIN.toString());
        List<String> actualRoles = roleCap.getValue().getRoleNamesToAdd();
        Assertions.assertEquals(expectedRoles, actualRoles);
    }

    @Test
    void testOrgProperties() throws Exception {
        HashMap<String, String> context = new HashMap<>();
        context.put("org_prop", "value");
        invitation.setContext(context);
        invitationService.handleServiceInvitation(invitationLink);
        verify(cspApiClient).patchOrgServiceRoles(tokenCap.capture(), orgIdCap.capture(),
                                                  emailCap.capture(), roleCap.capture());
        verify(organizationService).put(orgCap.capture());
        Map<String, String> orgProps = orgCap.getValue().getOrganizationProperties();
        Assertions.assertEquals(1, orgProps.size());
        Assertions.assertEquals("value", orgProps.get("org_prop"));
        List<String> expectedRoles =
                ImmutableList.of(VmbcRoles.CONSORTIUM_ADMIN.toString(),
                                 VmbcRoles.ORG_ADMIN.toString());
        List<String> actualRoles = roleCap.getValue().getRoleNamesToAdd();
        Assertions.assertEquals(expectedRoles, actualRoles);
    }

    @Test
    void testRolesAndProps() throws Exception {
        HashMap<String, String> context = new HashMap<>();
        context.put(Constants.INVITATION_ROLE, VmbcRoles.INFRA_ADMIN.toString());
        context.put("org_prop", "value");
        invitation.setContext(context);
        invitationService.handleServiceInvitation(invitationLink);
        verify(cspApiClient, times(1)).patchOrgServiceRoles(anyString(), any(UUID.class), anyString(),
                                                            any(CspPatchServiceRolesRequest.class));
        verify(cspApiClient).patchOrgServiceRoles(tokenCap.capture(), orgIdCap.capture(),
                                                  emailCap.capture(), roleCap.capture());
        verify(organizationService).put(orgCap.capture());
        Map<String, String> orgProps = orgCap.getValue().getOrganizationProperties();
        Assertions.assertEquals(1, orgProps.size());
        Assertions.assertEquals("value", orgProps.get("org_prop"));
        List<String> expectedRoles =
                ImmutableList.of(VmbcRoles.INFRA_ADMIN.toString(), VmbcRoles.CONSORTIUM_ADMIN.toString(),
                                 VmbcRoles.ORG_ADMIN.toString());
        List<String> actualRoles = roleCap.getValue().getRoleNamesToAdd();
        Assertions.assertEquals(expectedRoles, actualRoles);
    }

    @Test
    void testBadInvite() throws Exception {
        Assertions.assertThrows(BadRequestException.class, () -> invitationService.handleServiceInvitation("argle"));
        verify(cspApiClient, times(0)).patchOrgServiceRoles(anyString(), any(UUID.class), anyString(),
                                                            any(CspPatchServiceRolesRequest.class));
    }

    @Test
    void testBadOrg() throws Exception {
        when(authHelper.getOrganizationId()).thenReturn(UUID.fromString("5e93e41c-3d1c-43ac-bec7-a8c13670e72e"));
        Assertions.assertThrows(BadRequestException.class,
            () -> invitationService.handleServiceInvitation(invitationLink));
        verify(cspApiClient, times(0)).patchOrgServiceRoles(anyString(), any(UUID.class), anyString(),
                                                            any(CspPatchServiceRolesRequest.class));
    }

    @Test
    void testBadUser() throws Exception {
        when(cspApiClient.getUser(anyString())).thenReturn(null);
        Assertions.assertThrows(BadRequestException.class,
            () -> invitationService.handleServiceInvitation(invitationLink));
        verify(cspApiClient, times(0)).patchOrgServiceRoles(anyString(), any(UUID.class), anyString(),
                                                            any(CspPatchServiceRolesRequest.class));
    }



}