/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.dao.GenericDao;

/**
 * User Service.  Business logic for User management.
 */
@Component
public class UserService {

    private GenericDao genericDao;

    @Autowired
    public UserService(GenericDao genericDao) {
        this.genericDao = genericDao;
    }

    public User get(UUID id) {
        return genericDao.get(id, User.class);
    }

    public User put(User user) {
        return genericDao.put(user, null);
    }

    public List<User> list() {
        return genericDao.getAllByType(User.class);
    }

    public List<Organization> getOrganizations(User user) {
        return genericDao.getByParentId(user.getId(), Organization.class);
    }

    public Organization getDefaultOrganization(User user) {
        List<Organization> l = getOrganizations(user);
        return l.isEmpty() ? null : l.get(0);
    }

    public List<Consortium> getConsortiums(User user) {
        List<UUID> orgIds = getOrganizations(user).stream().map(o -> o.getId()).collect(Collectors.toList());
        return genericDao.getByParentList(orgIds, Consortium.class);
    }

    public Consortium getDefaultConsortium(User user) {
        List<Consortium> c = genericDao.getByParentId(user.getOrganization(), Consortium.class);
        return c.isEmpty() ? null : c.get(0);
    }

    /**
     * Select a user by email address.  Uses json query.
     * @param email address
     * @return user
     */
    public User getByEmail(String email) {
        String json = JSONObject.toJSONString(Collections.singletonMap("email", email));
        List<User> l = genericDao.getByJsonQuery(json, User.class);
        if (l.isEmpty()) {
            throw new NotFoundException(ErrorCode.USER_NOT_FOUND, email);
        }
        return l.get(0);

    }
}
