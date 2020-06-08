/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.ErrorCode;
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
    private OrganizationService organizationService;
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
        private final UUID orgId;

        public AgreementResponse(Agreement a) {
            this.id = a.getId();
            this.accepted = a.isAccepted();
            this.type = a.getType();
            this.firstName = a.getFirstName();
            this.lastName = a.getLastName();
            this.company = a.getCompany();
            this.acceptedOn = a.getAcceptedOn();
            this.orgId = a.getOrgId();

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
    public AgreementController(AgreementService arm, OrganizationService organizationService, AuthHelper authHelper,
                               ServiceContext serviceContext) {
        this.arm = arm;
        this.organizationService = organizationService;
        this.authHelper = authHelper;
        this.serviceContext = serviceContext;
    }

    /**
     * Get an agreement.
     * @param orgIdOptional orgIdOptional
     * @return Agreement
     */
    @RequestMapping(path = {"/api/organizations/{org_id}/agreements", "api/organizations/agreements"},
            method = RequestMethod.GET)
    @PreAuthorize("@authHelper.isAuthenticated()")
    public ResponseEntity<List<AgreementResponse>> getAgreementFromId(
            @PathVariable(name = "org_id", required = false) Optional<UUID> orgIdOptional) {
        UUID orgId;
        if (orgIdOptional.isPresent()) {
            if (authHelper.isServiceAdmin() || orgIdOptional.get() == authHelper.getOrganizationId()) {
                orgId = orgIdOptional.get();
            } else {
                throw new BadRequestException(ErrorCode.BAD_REQUEST);
            }
        } else {
            orgId = authHelper.getOrganizationId();
        }
        List<Agreement> agreementList = organizationService.getAgreements(orgId);
        List<AgreementResponse> agreementResponseList = agreementList.stream().map(AgreementResponse::new)
                .collect(Collectors.toList());

        return new ResponseEntity<>(agreementResponseList, HttpStatus.OK);
    }

    /**
     * Update an Agreement.
     * @param orgIdOptional orgIdOptional
     * @param requestBody Patch details.
     */
    @RequestMapping(path = {"/api/organizations/{org_id}/agreements", "api/organizations/agreements"},
            method = RequestMethod.POST)
    @PreAuthorize("@authHelper.isAuthenticated()")
    public ResponseEntity<Void> doPost(@PathVariable(name = "org_id", required = false) Optional<UUID> orgIdOptional,
                                        @RequestBody AgreementRequest requestBody) {
        UUID orgId;

        if (orgIdOptional.isPresent()) {
            if (authHelper.isServiceAdmin() || orgIdOptional.get() == authHelper.getOrganizationId()) {
                orgId = orgIdOptional.get();
            } else {
                throw new BadRequestException(ErrorCode.BAD_REQUEST);
            }
        } else {
            orgId = authHelper.getOrganizationId();
        }

        if (requestBody.isAccepted()
                && !(requestBody.getFirstName().isBlank())
                && !(requestBody.getLastName().isBlank())
                && !(requestBody.getCompany().isBlank())) {

            List<Agreement> agreementList = organizationService.getAgreements(orgId);

            Agreement agreement;

            if (!agreementList.isEmpty()) {
                agreement = agreementList.get(0);
            } else {
                agreement = arm.createAgreement();
            }

            agreement.setAccepted(requestBody.isAccepted());
            agreement.setFirstName(requestBody.getFirstName());
            agreement.setLastName(requestBody.getLastName());
            agreement.setCompany(requestBody.getCompany());
            agreement.setAcceptedOn(new Date());
            agreement.setOrgId(orgId);
            arm.updateAgreement(agreement);

            return new ResponseEntity<>(HttpStatus.OK);
        } else {
            throw new BadRequestException(ErrorCode.BAD_REQUEST);
        }

    }
}
