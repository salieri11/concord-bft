/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

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

/**
 * A servlet which manages all GET/POST/PATCH requests related to wallet API of helen.
 */
@Controller
public class WalletController {
    private static final Logger logger = LogManager.getLogger(ProfileController.class);

    KeystoreService krm;
    ProfilesService prm;

    @Autowired
    public WalletController(ProfilesService prm, KeystoreService krm) {
        this.krm = krm;
        this.prm = prm;
    }

    /**
     * Get all wallet addresses for a specific user.
     * @param userId User Id
     * @return the list of wallet addresses
     */
    @RequestMapping(path = "/api/users/{user_id}/wallet", method = RequestMethod.GET)
    @PreAuthorize("@authHelper.isAuthenticated()")
    public ResponseEntity<List<String>> getWalletsForUser(@PathVariable("user_id") UUID userId) {
        List<String> wallets =
                krm.getWalletsForUser(userId).stream().map(Keystore::getAddress).collect(Collectors.toList());
        return new ResponseEntity<>(wallets, HttpStatus.OK);
    }

    /**
     * Get wallet from address for a specific user.
     * @param userId User Id
     * @param address public address of wallet, please note there is no leading "0x" in address
     * @return the wallet
     */
    @RequestMapping(path = "/api/users/{user_id}/wallet/{address}", method = RequestMethod.GET)
    @PreAuthorize("@authHelper.isAuthenticated()")
    public ResponseEntity<String> getWalletFromAddress(@PathVariable("user_id") UUID userId,
                                                   @PathVariable("address") String address) {
        return new ResponseEntity<>(krm.getWalletByAddress(address), HttpStatus.OK);
    }
}
