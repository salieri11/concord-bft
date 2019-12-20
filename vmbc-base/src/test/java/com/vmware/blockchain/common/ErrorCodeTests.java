/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.autoconfigure.ImportAutoConfiguration;
import org.springframework.boot.autoconfigure.context.MessageSourceAutoConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

/**
 * Test error codes.
 */
@ExtendWith(SpringExtension.class)
@ImportAutoConfiguration(MessageSourceAutoConfiguration.class)
public class ErrorCodeTests {


    @Test
    public void getMessageKnownErrorCode() throws Exception {
        Exception e = new VmbcException(ErrorCodeType.BAD_NUMBER_FORMAT);
        String errorMessage = e.getMessage();
        Assert.assertNotNull("helen.bad.number.format", errorMessage);
    }


    @Test
    public void getLocalizedMessage() throws Exception {
        Exception e = new VmbcException(ErrorCodeType.INVALID_BLOCK_REQUEST);
        String errorMessage = e.getLocalizedMessage();
        Assert.assertNotNull("Error message can't be null", errorMessage);
        Assert.assertEquals("Error message doesn't match what is expected. These should not be localized",
                            "helen.invalid.request.choose.correct.block.numbers.range.or.a.block.hash",
                            errorMessage.trim());
    }

    //look into this test
    /**
    @Test
    public void getForThrowable() throws Exception {
        String message = "Something wrong with entity";
        HelenException e = new HelenException(ErrorCodeType.ENTITY_ISSUES, new Throwable(message),
                                              "entityIssues");

        String errorMessage = String.format("helen.issues.with.entity %s.", "entityIssues");
        Assert.assertEquals(errorMessage, e.getMessage());
    }
*/
}
