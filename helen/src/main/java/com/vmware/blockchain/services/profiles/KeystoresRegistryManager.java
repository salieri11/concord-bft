/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Optional;

import javax.transaction.Transactional;

import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * This class manages all persistence related operations related to Keystore management API.
 */
@Component
@Transactional
public class KeystoresRegistryManager {

    @Autowired
    private KeystoreRepository keystoreRepository;

    @Autowired
    private UserRepository userRepository;

    /**
     * store the wallet into DB.
     * @param address the address of wallet
     * @param wallet wallet in json format
     * @return if the wallet successfully stored in DB
     */
    public boolean storeKeystore(String userName, String address, String wallet) {
        Optional<User> oUser = userRepository.findUserByEmail(userName);
        if (!oUser.isPresent()) {
            return false;
        }
        Optional<Keystore> oKeystore = keystoreRepository.findById(address);
        if (oKeystore.isPresent()) {
            return false;
        }
        Keystore keystore = new Keystore();
        keystore.setAddress(address);
        keystore.setWallet(wallet);
        keystore.setUser(oUser.get());
        keystoreRepository.save(keystore);
        oUser.get().addKeystore(keystore);
        userRepository.save(oUser.get());
        return true;
    }

    /**
     * retrieve wallet by address.
     * @param address wallet address
     * @return wallet
     * @throws ParseException bad wallet format
     */
    public JSONObject getWalletByAddress(String address) throws ParseException {
        Optional<Keystore> oKeystore = keystoreRepository.findById(address);
        if (!oKeystore.isPresent()) {
            return new JSONObject();
        } else {
            String wallet = oKeystore.get().getWallet();
            return (JSONObject) new JSONParser().parse(wallet);
        }
    }
}
