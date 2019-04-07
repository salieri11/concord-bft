/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.dao.GenericDao;

/**
 * This class manages all persistence related operations related to Keystore management API.
 */
@Service
public class KeystoreService {
    private static final Logger logger = LogManager.getLogger(ProfileController.class);
    @Autowired
    private GenericDao genericDao;

    @Autowired
    private UserService userService;

    /**
     * store the wallet into DB.
     * @param address the address of wallet
     * @param wallet wallet in json format
     * @return if the wallet successfully stored in DB
     */
    public boolean storeKeystore(String userName, String address, String wallet) {
        try {
            final User user = userService.getByEmail(userName);
            String json = JSONObject.toJSONString(Collections.singletonMap("address", address));
            List<Keystore> keystores = genericDao.getByJsonQuery(json, Keystore.class);
            Keystore keystore = keystores.isEmpty() ? new Keystore() : keystores.get(0);
            keystore.setAddress(address);
            keystore.setWallet(wallet);
            keystore.setUser(user.getId());
            genericDao.put(keystore, null);
            return true;
        } catch (NotFoundException e) {
            return false;
        }
    }

    /**
     * retrieve wallets for user.
     * TODO: Does this belong in UserService?
     * @param id user ID
     * @return wallet
     */
    public List<Keystore> getWalletsForUser(UUID id) {
        return genericDao.getByParentId(id, Keystore.class);
    }

    /**
     * retrieve wallet by address.
     * @param address wallet address
     * @return wallet
     */
    public String getWalletByAddress(String address) {
        String json = JSONObject.toJSONString(Collections.singletonMap("address", address));
        List<Keystore> keystores = genericDao.getByJsonQuery(json, Keystore.class);
        return keystores.isEmpty() ? "{}" : keystores.get(0).getWallet();
    }
}
