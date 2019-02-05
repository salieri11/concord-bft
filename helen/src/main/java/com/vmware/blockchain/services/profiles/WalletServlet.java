/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.json.simple.parser.ParseException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.vmware.blockchain.services.ethereum.ApiHelper;

/**
 * A servlet which manages all GET/POST/PATCH requests related to wallet API of helen.
 */
@Controller
public class WalletServlet {
    private static final Logger logger = LogManager.getLogger(ProfileManager.class);

    KeystoresRegistryManager krm;
    ProfilesRegistryManager prm;

    @Autowired
    public WalletServlet(ProfilesRegistryManager prm, KeystoresRegistryManager krm) {
        this.krm = krm;
        this.prm = prm;
    }

    /**
     * Get all wallet addresses for a specific user.
     * @param userId User Id
     * @return the list of wallet addresses
     */
    @RequestMapping(path = "/api/users/{user_id}/wallet", method = RequestMethod.GET)
    public ResponseEntity<JSONAware> getWalletsForUser(@PathVariable("user_id") String userId) {
        JSONObject user = prm.getUserWithId(userId);
        if (user.isEmpty()) {
            return new ResponseEntity<>(new JSONArray(), HttpStatus.NOT_FOUND);
        } else {
            HttpStatus responseStatus;
            JSONArray responseJson;
            try {
                responseJson = krm.getWalletsForUser(userId);
                responseStatus = HttpStatus.OK;
            } catch (ParseException e) {
                logger.warn("Error while retrieving client wallets", e);
                responseJson = new JSONArray();
                responseStatus = HttpStatus.NOT_FOUND;
            }
            return new ResponseEntity<>(responseJson, responseStatus);
        }
    }

    /**
     * Get wallet from address for a specific user.
     * @param userId User Id
     * @param address public address of wallet, please note there is no leading "0x" in address
     * @return the wallet
     */
    @RequestMapping(path = "/api/users/{user_id}/wallet/{address}", method = RequestMethod.GET)
    public ResponseEntity<JSONAware> getWalletFromAddress(@PathVariable("user_id") String userId,
                                                   @PathVariable("address") String address) {
        JSONObject user = prm.getUserWithId(userId);
        if (user.isEmpty()) {
            return new ResponseEntity<>(new JSONObject(), HttpStatus.NOT_FOUND);
        } else {
            HttpStatus responseStatus;
            JSONObject responseJson;
            try {
                responseJson = krm.getWalletByAddress(address);
                responseStatus = HttpStatus.OK;
            } catch (ParseException e) {
                logger.warn("Error while retrieving client wallet", e);
                responseJson = ApiHelper.errorJson(e.getMessage());
                responseStatus = HttpStatus.BAD_REQUEST;
            }
            return new ResponseEntity<>(responseJson, responseStatus);
        }
    }
}
