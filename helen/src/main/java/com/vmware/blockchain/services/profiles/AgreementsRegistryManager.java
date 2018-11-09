/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Optional;

import javax.transaction.Transactional;

import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * This class manages all persistence related operations related to Agreement management API.
 */
@Component
@Transactional
public class AgreementsRegistryManager {

    @Autowired
    private AgreementRepository agreementRepository;

    /* Needed for spring */
    protected AgreementsRegistryManager() {}

    /**
     * Get an agreement from the DB, and convert to JSON.
     * @param id Agreement ID
     * @return JSON Object
     */
    public JSONObject getAgreementWithId(String id) {
        JSONObject json = new JSONObject();
        Optional<Agreement> oAgreement = agreementRepository.findById(Long.parseLong(id));

        if (oAgreement.isPresent()) {
            Agreement a = oAgreement.get();
            Boolean accepted = a.getAcceptance();
            json.put("id", a.getId());
            json.put("type", a.getType());
            json.put("accepted", accepted);
            json.put("first_name", a.getFirstName());
            json.put("last_name", a.getLastName());
            json.put("company", a.getCompany());
            json.put("accepted_on", a.getAcceptedOn());

            // Don't send legal agreement if already accepted
            // it's too heavy
            if (accepted == Boolean.FALSE) {
                json.put("content", a.getContent());
            }
        }

        return json;
    }

    /**
     * Update the agreement in the DB from the json request.
     *
     * @param id Agreement ID
     * @param request JSON Object with new agreement info
     * @return updated agreement JSON
     */
    public JSONObject updateAgreement(String id, JSONObject request) {
        JSONObject json = new JSONObject();
        Optional<Agreement> oAgreement = agreementRepository.findById(Long.parseLong(id));

        if (oAgreement.isPresent()) {
            Agreement agreement = oAgreement.get();
            if (request.get("accepted") == Boolean.TRUE) {
                agreement.accepted();
                agreement.setFirstName(request.get("first_name").toString());
                agreement.setLastName(request.get("last_name").toString());
                agreement.setCompany(request.get("company").toString());
                agreement.setAcceptedOn();
            }
            agreementRepository.save(agreement);
        }

        return json;
    }
}
