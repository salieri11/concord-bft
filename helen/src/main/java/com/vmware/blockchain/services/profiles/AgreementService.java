/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.List;
import java.util.UUID;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Component;
import org.springframework.util.StreamUtils;

import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.dao.GenericDao;

/**
 * This class manages all persistence related operations related to Agreement management API.
 */
@Component
@Transactional
public class AgreementService {

    private GenericDao genericDao;

    @Autowired
    public AgreementService(GenericDao genericDao) {
        this.genericDao = genericDao;
    }

    /**
     * Get the agreement from the database.
     *
     * <p>Note: At the moment there is only one agreement, so ignore the UUID.  At some point, this will
     * be an agreement per organization.
     */
    public Agreement getAgreementWithId(UUID id) {
        List<Agreement> l = genericDao.getAllByType(Agreement.class);
        if (l.isEmpty()) {
            throw new NotFoundException(ErrorCode.AGREEMENT_NOT_FOUND);
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
            return genericDao.put(a, null);
        } catch (IOException e) {
            throw new NotFoundException(e, ErrorCode.AGREEMENT_NOT_FOUND);
        }
    }

    private String fromFile(String location) throws IOException {
        ClassPathResource classPathResource = new ClassPathResource(location);
        return StreamUtils.copyToString(classPathResource.getInputStream(), Charset.forName("utf-8"));
    }

}
