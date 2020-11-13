/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.vmware.blockchain.auth.AuthHelper;

/**
 * A servlet which manages all GET/POST/PATCH requests related to user management API of helen.
 */
@Controller
public class ProfileController {

    private static final long serialVersionUID = 1L;
    private static final Logger logger = LogManager.getLogger(ProfileController.class);

    private ProfilesService prm;
    private UserService userSerivce;
    private DefaultProfiles profiles;
    private AuthHelper authHelper;

    @Autowired
    public ProfileController(ProfilesService prm,
                             UserService userService,
                             DefaultProfiles profiles,
                             AuthHelper authHelper) {
        this.prm = prm;
        this.userSerivce = userService;
        this.profiles = profiles;
        this.authHelper = authHelper;
    }


    /**
     * Get user from ID.
     * @param userId User Id
     * @return the user
     */
    @RequestMapping(path = "/api/users/{user_id}", method = RequestMethod.GET)
    @PreAuthorize("@authHelper.isSystemAdmin()")
    public ResponseEntity<UsersGetResponse> getUserFromId(@PathVariable("user_id") String userId) {
        return new ResponseEntity<>(prm.getReponse(prm.getUserWithId(userId)), HttpStatus.OK);
    }


}
