/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.List;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;
import org.springframework.util.StreamUtils;

import com.vmware.blockchain.common.ErrorCodeType;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.dao.GenericDao;

/**
 * This class manages all persistence related operations related to Agreement management API.
 */
@Service
public class AgreementService {

    private GenericDao genericDao;
    private OrganizationService organizationService;

    @Autowired
    public AgreementService(GenericDao genericDao) {
        this.genericDao = genericDao;
        this.organizationService = new OrganizationService(this.genericDao);
    }

    /**
     * Get the agreement from the database.
     */
    public Agreement getAgreementWithId(UUID orgId) {
        List<Agreement> l = organizationService.getAgreements(orgId);
        if (l.isEmpty()) {
            return null;
        }
        return l.get(0);
    }

    /**
     * Update the agreement.
     *
     */
    public void updateAgreement(Agreement agreement) {
        genericDao.put(agreement, null);
    }


    /**
     * Create a new agreement.  Read the Term of Service from the resource.
     * Again note: for the moment there is only one of these.
     */
    public Agreement createAgreement() {
        try {
            String termsOfService = fromFile("tos.txt");
            Agreement a = new Agreement();
            a.setAccepted(false);
            a.setType("PRE-RELEASE SERVICE OFFERING");
            a.setContent(termsOfService);
            return a;
        } catch (IOException e) {
            throw new NotFoundException(ErrorCodeType.AGREEMENT_NOT_FOUND, e);
        }
    }

    private String fromFile(String location) throws IOException {
        ClassPathResource classPathResource = new ClassPathResource(location);
        return StreamUtils.copyToString(classPathResource.getInputStream(), Charset.forName("utf-8"));
    }
}
