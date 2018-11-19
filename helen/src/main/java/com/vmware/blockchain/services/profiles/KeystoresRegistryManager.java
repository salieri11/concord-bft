/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Optional;

import javax.transaction.Transactional;

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

    /**
     * store the wallet into DB.
     * @param address the address of wallet
     * @param wallet wallet in json format
     * @return if the wallet successfully stored in DB
     */
    public boolean storeKeystore(String address, String wallet) {
        Optional<Keystore> oKeystore = keystoreRepository.findById(address);
        if (oKeystore.isPresent()) {
            return false;
        }
        Keystore keystore = new Keystore();
        keystore.setAddress(address);
        keystore.setWallet(wallet);
        keystoreRepository.save(keystore);
        return true;
    }
}
