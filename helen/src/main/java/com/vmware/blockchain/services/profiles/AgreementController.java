/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Date;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.security.ServiceContext;

import lombok.Getter;
import lombok.Setter;
import lombok.Value;

/**
 * A Controller that manages all GET/POST/PATCH requests related to user management API of helen.
 */
@Controller
public class AgreementController  {

    private static final Logger logger = LogManager.getLogger(AgreementController.class);

    private AgreementService arm;
    private AuthHelper authHelper;
    private ServiceContext serviceContext;

    @Value
    private static class AgreementResponse {
        private final UUID id;
        private final boolean accepted;
        private final String type;
        private final String firstName;
        private final String lastName;
        private final String company;
        private final Date acceptedOn;
        private final String content;

        public AgreementResponse(Agreement a) {
            this.id = a.getId();
            this.accepted = a.isAccepted();
            this.type = a.getType();
            this.firstName = a.getFirstName();
            this.lastName = a.getLastName();
            this.company = a.getCompany();
            this.acceptedOn = a.getAcceptedOn();
            if (accepted) {
                this.content = "";
            } else {
                this.content = a.getContent();
            }
        }
    }

    @Getter
    @Setter
    private static class AgreementRequest {
        private boolean accepted;
        private String type;
        private String firstName;
        private String lastName;
        private String company;
    }

    @Autowired
    public AgreementController(AgreementService arm, AuthHelper authHelper, ServiceContext serviceContext) {
        this.arm = arm;
        this.authHelper = authHelper;
        this.serviceContext = serviceContext;
    }

    /**
     * Get an agreement.
     * @param id Id
     * @return Agreement
     */
    @RequestMapping(path = "/api/agreements/{id}", method = RequestMethod.GET)
    public ResponseEntity<AgreementResponse> getAgreementFromId(@PathVariable("id") int id) {
        AgreementResponse r = new AgreementResponse(arm.getAgreementWithId(null));
        return new ResponseEntity<>(r, HttpStatus.OK);
    }

    /**
     * Update an Agreement.
     * @param id Id
     * @param requestBody Patch details.
     */
    @RequestMapping(path = "/api/agreements/{id}", method = RequestMethod.PATCH)
    public ResponseEntity<Void> doPatch(@PathVariable(name = "id") int id, @RequestBody AgreementRequest requestBody) {
        // There is a special case for the near term.  In the current flow, the agreement is accepted before
        // the user is logged in. Set the auth context to an anonymous user, if it is currently empty
        if (authHelper.getUserId() == null) {
            serviceContext.setAnonymousContext();
        }

        Agreement a = arm.getAgreementWithId(null);
        if (requestBody.isAccepted()) {
            a.setAccepted(requestBody.isAccepted());
            a.setFirstName(requestBody.getFirstName());
            a.setLastName(requestBody.getLastName());
            a.setCompany(requestBody.getCompany());
            a.setAcceptedOn(new Date());
            arm.updateAgreement(a);
        }

        if (authHelper.hasAnyAuthority(Roles.ANONYMOUS.getName())) {
            serviceContext.clearServiceContext();
        }
        return new ResponseEntity<>(HttpStatus.OK);
    }

}
