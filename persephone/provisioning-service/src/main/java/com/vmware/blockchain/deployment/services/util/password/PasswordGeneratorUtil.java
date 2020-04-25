/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.util.password;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.text.RandomStringGenerator;

/**
 * Util class to generate random password.
 * https://pubs.vmware.com/vsphere-51/index.jsp?topic=%2Fcom.vmware.vsphere.security.doc%2FGUID-DC96FFDB-F5F2-43EC-8C73-05ACDAE6BE43.html
 */
public class PasswordGeneratorUtil {

    /**
     * Static utility to generate password.
     *
     * @return string.
     */
    public static String generateCommonTextPassword() {
        String pwString = generateRandomLowerCaseAlphabet(4).concat(generateRandomNumbers(4))
                .concat(generateRandomUpperCaseAlphabet(4))
                .concat("-")
                .concat("_");
        List<Character> pwChars = pwString.chars()
                .mapToObj(data -> (char) data)
                .collect(Collectors.toList());
        Collections.shuffle(pwChars);
        String password = pwChars.stream()
                .collect(StringBuilder::new, StringBuilder::append, StringBuilder::append)
                .toString();
        return generateRandomLowerCaseAlphabet(1) + password;
    }

    private static String generateRandomCharacters(int length, int startAscii, int endAscii) {
        RandomStringGenerator pwdGenerator = new RandomStringGenerator.Builder().withinRange(startAscii, endAscii)
                .build();
        return pwdGenerator.generate(length);
    }

    private static String generateRandomLowerCaseAlphabet(int length) {
        return generateRandomCharacters(length, 97, 122);
    }

    private static String generateRandomUpperCaseAlphabet(int length) {
        return generateRandomCharacters(length, 65, 90);
    }

    private static String generateRandomNumbers(int length) {
        return generateRandomCharacters(length, 48, 57);
    }
}
