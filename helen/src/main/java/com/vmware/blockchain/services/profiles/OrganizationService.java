/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.List;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.dao.GenericDao;

/**
 * Organization Service.  Provided data access to organization entities.
 */
@Component
public class OrganizationService {

    private GenericDao genericDao;

    @Autowired
    public OrganizationService(GenericDao genericDao) {
        this.genericDao = genericDao;
    }

    public Organization get(UUID id) {
        return genericDao.get(id, Organization.class);
    }

    public Organization put(Organization org) {
        return genericDao.put(org, null);
    }

    public List<Organization> list() {
        return genericDao.getAllByType(Organization.class);
    }

    public List<User> getUsers(UUID id)  {
        return genericDao.getByParentId(id, User.class);
    }

    public List<Consortium> getConsortiums(UUID id) {
        return genericDao.getByParentId(id, Consortium.class);
    }

    public List<Agreement> getAgreements(UUID id) {
        return genericDao.getByParentId(id, Agreement.class);
    }
}
