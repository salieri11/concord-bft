/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.invitation;

import java.io.IOException;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.google.common.collect.ImmutableList;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.csp.CspCommon;
import com.vmware.blockchain.common.csp.CspCommon.CspPatchServiceRolesRequest;
import com.vmware.blockchain.common.csp.CspConstants;
import com.vmware.blockchain.common.csp.api.client.CspApiClient;
import com.vmware.blockchain.common.csp.exception.CspApiException;
import com.vmware.blockchain.services.profiles.Organization;
import com.vmware.blockchain.services.profiles.OrganizationService;
import com.vmware.blockchain.services.profiles.Roles;

/**
 * Invitation controller.  Handles the call backs from CSP.
 */
@Service
public class InvitationService {
    static final Logger logger = LogManager.getLogger(InvitationService.class);

    private CspApiClient cspApiClient;
    private AuthHelper authHelper;
    private String serviceDefinitionLink;
    private OrganizationService orgService;

    @Autowired
    public InvitationService(CspApiClient cspApiClient,
                             AuthHelper authHelper,
                             OrganizationService orgService,
                             @Value("${vmbc.service.id:#null}") String serviceId) {
        this.cspApiClient = cspApiClient;
        this.authHelper = authHelper;
        this.orgService = orgService;
        this.serviceDefinitionLink = CspConstants.CSP_SERVICE_DEFINITION + "/external/" + serviceId;
    }

    /**
     * Handle a service invitation.  Called from the invitaion controller, so authentication is set up.
     */
    public void handleServiceInvitation(String invitationLink) throws IOException {


        logger.info("Handling service invitation {}", invitationLink);
        // first let's get the invitation
        try {
            CspCommon.CspServiceInvitation invitation = cspApiClient.getInvitation(invitationLink);

            Organization org = orgService.get(authHelper.getOrganizationId());

            if (org == null) {
                throw new BadRequestException(ErrorCode.BAD_REQUEST);
            }

            org.setOrganizationProperties(invitation.getContext());

            orgService.put(org);

            String orgLink = CspConstants.CSP_ORG_API + "/" + authHelper.getOrganizationId().toString();


            // Let's be sure this is the right org and right service
            if (!invitation.getOrgLink().equals(orgLink)
                || !serviceDefinitionLink.equals(invitation.getServiceDefinitionLink())) {
                throw new BadRequestException(ErrorCode.INVALID_INVITATION);
            }
            // Based on the context, determine what roles need to be added.
            // Right now the only thing we support is being a consortium owner.  At some point we
            // need to support invitation to a consortium.  This will be in the invitation's context.
            CspCommon.CspPatchServiceRolesRequest body = new CspPatchServiceRolesRequest();
            body.setServiceDefinitionLink(serviceDefinitionLink);
            List<String> roles = ImmutableList.of(Roles.CONSORTIUM_ADMIN.toString(), Roles.ORG_ADMIN.toString());
            body.setRoleNamesToAdd(roles);
            cspApiClient.patchOrgServiceRoles(authHelper.getAuthToken(), authHelper.getOrganizationId(),
                                              authHelper.getEmail(), body);
        } catch (CspApiException e) {
            // CSP exceptions are likely bad parameters or some such
            throw new BadRequestException(e, ErrorCode.INVALID_INVITATION);
        }
    }

}
