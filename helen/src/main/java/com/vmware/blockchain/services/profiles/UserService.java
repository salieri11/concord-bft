/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.dao.GenericDao;

/**
 * User Service.  Business logic for User management.
 */
@Service
public class UserService {

    private GenericDao genericDao;

    @Autowired
    public UserService(GenericDao genericDao) {
        this.genericDao = genericDao;
    }

    public User get(UUID id) {
        return fixupRoles(genericDao.get(id, User.class));
    }

    public User put(User user) {
        return genericDao.put(fixupRoles(user), null);
    }

    public User merge(User user, Consumer<User> merger) {
        return genericDao.mergeWithRetry(fixupRoles(user), User.class, merger);
    }

    public List<User> list() {
        return fixupRoles(genericDao.getAllByType(User.class));
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
        return fixupRoles(l.get(0));

    }

    // This does not have the serviceRoles, only the old roles.  Convert to the new roles.
    private User fixupRoles(User user) {
        if (user.getServiceRoles() == null && user.getRoles() != null) {
            user.setServiceRoles(
                    user.getRoles().stream().map(r -> VmbcRoles.get(r.getName())).collect(Collectors.toList()));
            user.setRoles(null);
        }
        return user;
    }

    private List<User> fixupRoles(List<User> users) {
        for (User user : users) {
            fixupRoles(user);
        }
        return users;
    }
}
