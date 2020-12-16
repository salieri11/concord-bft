/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.invitation;

import java.io.IOException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.util.UriComponentsBuilder;

import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.ErrorCodeType;
import com.vmware.blockchain.common.csp.CspConstants;

/**
 * Inviation Controller.  This handles the redirect coming from the invitatoin flow.
 */
@RestController
public class InvitationController {
    private static Logger logger = LogManager.getLogger(InvitationController.class);
    private AuthHelper authHelper;
    private InvitationService invitationService;

    @Autowired
    public InvitationController(AuthHelper authHelper, InvitationService invitationService) {
        this.authHelper = authHelper;
        this.invitationService = invitationService;
    }

    /**
     * This is the invitation return from Oauth2Controller.
     */
    @RequestMapping(method = RequestMethod.GET, path = Constants.AUTH_INVITATION)
    @PreAuthorize("@authHelper.isCspOrgOwner()")
    public void handleInvitation(HttpServletRequest httpRequest,
                                 HttpServletResponse httpResponse) throws IOException {

        HttpSession session = httpRequest.getSession(false);

        // We shouldn't be here with a null session
        if (session == null) {
            throw new BadRequestException(ErrorCodeType.INVALID_INVITATION);
        }
        // See if there is a service invitation
        String serviceInvitation = (String) httpRequest.getSession().getAttribute(Constants.CSP_INVITATION_LINK);
        if (serviceInvitation == null) {
            throw new BadRequestException(ErrorCodeType.INVALID_INVITATION);
        }
        // remove the property, just to be safe
        session.removeAttribute(Constants.CSP_INVITATION_LINK);
        logger.info("Invitation: {}", serviceInvitation);
        // Handle the invitation.  At the moment this adds constortium admin privilege.
        invitationService.handleServiceInvitation(serviceInvitation);

        // redirect back through the loggin flow.  We need this so the user gets the new permisuons.
        String orgLink = String.format("%s/%s", CspConstants.CSP_ORG_API, authHelper.getOrganizationId().toString());
        String redirect = UriComponentsBuilder.fromUriString(Constants.AUTH_LOGIN)
                .queryParam(Constants.CSP_ORG_LINK, orgLink)
                .queryParam(Constants.NEW_USER_PARAM, "new").build().toString();
        authHelper.evictToken();
        httpResponse.sendRedirect(redirect);
    }

}
