/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.dao.GenericDao;

/**
 * Consortium service.  Methods to get Consortiums by various means.
 */
@Service
public class ConsortiumService {
    private final Logger logger = LogManager.getLogger(ConsortiumService.class);

    private GenericDao genericDao;
    private AuthHelper authHelper;

    @Autowired
    public ConsortiumService(GenericDao genericDao, AuthHelper authHelper) {
        this.genericDao = genericDao;
        this.authHelper = authHelper;
    }

    public Consortium get(UUID id) {
        return genericDao.get(id, Consortium.class);
    }

    public Consortium put(Consortium c) {
        return genericDao.put(c, null);
    }

    /**
     * list consortiums to which this user has access.
     * @return List of consoritums user can see
     */
    public List<Consortium> list() {
        if (authHelper.hasAnyAuthority(VmbcRoles.systemAdmin())) {
            return genericDao.getAllByType(Consortium.class);
        } else {
            return genericDao.getByParentId(authHelper.getOrganizationId(), Consortium.class);
        }
    }

    /**
     * remove an organization from the Consortium.
     * @param c     Consortium
     * @param org   Org ID to remove
     * @throws IllegalArgumentException Removiing consortium owner org
     */
    public void removeOrganization(Consortium c, UUID org) {
        if (c.getOrganization().equals(org)) {
            throw new IllegalArgumentException(ErrorCode.BAD_ORG_REMOVE);
        }
        genericDao.deleteRelation(c.getId(), org);
        genericDao.deleteRelation(org, c.getId());
    }

    public void addOrganization(Consortium c, UUID org) {
        genericDao.saveBiDirectionalRelation(c.getId(), org);
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
