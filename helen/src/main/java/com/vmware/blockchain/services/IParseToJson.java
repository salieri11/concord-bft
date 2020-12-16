/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services;

import java.util.UUID;

import org.json.simple.JSONAware;

import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.ErrorCodeType;
import com.vmware.concord.Concord;

/**
 * Interface to implement a parseToJson response.  This needs to go away some day.
 * There are two implementations provided, one with the blockchain id, one without.
 * You can implement one or the other.
 */
public interface IParseToJson {

    /**
     * Return a response to json when the blockchain id is not needed.
     */
    default JSONAware parseToJson(Concord.ConcordResponse concordResponse) {
        return parseToJson(null, concordResponse);
    }

    /**
     * Return a response in Json, where the blockchain id is needed by the response.
     * Throw a "Unsupported Operation" if blockchain is null, to prevent an infinite recursion
     * if no override is implemented.
     */
    default JSONAware parseToJson(UUID blockchain, Concord.ConcordResponse concordResponse) {
        if (blockchain == null) {
            throw new BadRequestException(ErrorCodeType.BLOCKCHAIN_UNSPECIFIED);
        }
        return parseToJson(concordResponse);
    }

}
