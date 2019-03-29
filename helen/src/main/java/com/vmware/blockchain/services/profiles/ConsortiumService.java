/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.dao.GenericDao;

/**
 * Consortium service.  Methods to get Consortiums by various means.
 */
@Component
public class ConsortiumService {

    private GenericDao genericDao;

    @Autowired
    public ConsortiumService(GenericDao genericDao) {
        this.genericDao = genericDao;
    }

    public Consortium get(UUID id) {
        return genericDao.get(id, Consortium.class);
    }

    public Consortium put(Consortium c) {
        return genericDao.put(c, null);
    }

    public List<Consortium> list() {
        return genericDao.getAllByType(Consortium.class);
    }

    public void addOrganization(Consortium c, Organization org) {
        genericDao.saveBiDirectionalRelation(c.getId(), org.getId());
    }

    public List<Organization> getOrganizations(UUID id) {
        return genericDao.getByParentId(id, Organization.class);
    }

    // List all users in this consortium.  Find all the orgs, and get all the users in those orgs.
    public List<User> getUsers(UUID id) {
        List<UUID> orgIds = getOrganizations(id).stream().map(o -> o.getId()).collect(Collectors.toList());
        return genericDao.getByParentList(orgIds, User.class);
    }
}
