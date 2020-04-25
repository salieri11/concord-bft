/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.util.password;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Test generate random password.
 */
public class PasswordGeneratorUtilTest {

    @Test
    public void testPasswordGenerated() {
        String password = PasswordGeneratorUtil.generateCommonTextPassword();
        int lowerCaseCount = 0;
        int upperCaseCount = 0;
        int numberCount = 0;
        int spclCharCount = 0;

        for (char ch : password.toCharArray()) {
            if (ch >= 'A' && ch <= 'Z') {
                upperCaseCount++;
            } else if (ch >= 'a' && ch <= 'z') {
                lowerCaseCount++;
            } else if (ch >= '0' && ch <= '9') {
                numberCount++;
            } else {
                spclCharCount++;
            }
        }

        assertTrue("Password validation failed: lower ", lowerCaseCount == 5);
        assertTrue("Password validation failed: upper ", upperCaseCount == 4);
        assertTrue("Password validation failed: number ", numberCount == 4);
        assertTrue("Password validation failed: spcl ", spclCharCount == 2);

        assertTrue("Starting sequence ", Character.isLowerCase(password.charAt(0)));
    }

    @Test
    public void testRandomPasswordGenerated() {
        assertFalse("Starting sequence ", PasswordGeneratorUtil.generateCommonTextPassword()
                .equals(PasswordGeneratorUtil.generateCommonTextPassword()));
    }
}