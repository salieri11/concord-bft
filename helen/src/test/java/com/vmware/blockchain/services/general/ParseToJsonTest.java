/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.general;

import java.util.UUID;

import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.services.IParseToJson;
import com.vmware.concord.Concord;
import com.vmware.concord.Concord.ConcordResponse;


/**
 * Do some validation on the IParseToJson interface.
 */
@ExtendWith(SpringExtension.class)
public class ParseToJsonTest {

    private static class NoDefault implements IParseToJson {
        @Mock
        Concord.ConcordResponse response;

        public void callit() {
            parseToJson(response);
        }
    }

    private static class NoBlockchain implements IParseToJson {
        @Mock
        Concord.ConcordResponse response;



        @Override
        public JSONAware parseToJson(ConcordResponse concordResponse) {
            return new JSONObject();
        }



        public JSONAware callit() {
            return parseToJson(UUID.randomUUID(), response);
        }
    }


    private static class WithBlockchain implements IParseToJson {
        @Mock
        Concord.ConcordResponse response;



        @Override
        public JSONAware parseToJson(UUID blockchain, ConcordResponse concordResponse) {
            return new JSONObject();
        }



        public JSONAware callit() {
            return parseToJson(response);
        }
    }

    @Test
    void testRecursion() {
        NoDefault nd = new NoDefault();
        Assertions.assertThrows(BadRequestException.class, () -> nd.callit());
    }

    @Test
    void testNoBlockchain() {
        NoBlockchain nb = new NoBlockchain();
        Assertions.assertNotNull(nb.callit());
    }

    @Test
    void testWithBlockchain() {
        WithBlockchain wb = new WithBlockchain();
        Assertions.assertNotNull(wb.callit());
    }


}
