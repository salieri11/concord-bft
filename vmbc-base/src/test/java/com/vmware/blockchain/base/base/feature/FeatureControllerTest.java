/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.base.base.feature;

import org.ff4j.FF4j;
import org.ff4j.exception.FeatureNotFoundException;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;

/**
 * Feature controller tests.
 */
public class FeatureControllerTest {

    @Test
    public void my_first_test() {
        // Given: file exist
        Assertions.assertNotNull(getClass().getClassLoader().getResourceAsStream("ff4j.xml"));
        // When: init
        FF4j ff4j = new FF4j("ff4j.xml");
        // Then
        Assertions.assertEquals(5, ff4j.getFeatures().size());
        Assertions.assertTrue(ff4j.exist("hello"));
        Assertions.assertTrue(ff4j.check("hello"));

        // Usage
        if (ff4j.check("hello")) {
            // hello is toggle on
            System.out.println("Hello World !!");
        }

        // When: Toggle Off
        ff4j.disable("hello");
        // Then: expected to be off
        Assertions.assertFalse(ff4j.check("hello"));
    }

    @Test
    public void how_does_AutoCreate_Works() {
        // Given: feature not exist
        FF4j ff4j = new FF4j("ff4j.xml");
        Assertions.assertFalse(ff4j.exist("does_not_exist"));

        // When: use it
        try {
            if (ff4j.check("does_not_exist")) {
                Assertions.fail();
            }
        } catch (FeatureNotFoundException fnfe) {
            // Then: exception
            System.out.println("By default, feature not found throw exception");
        }

        // When: using autocreate
        ff4j.setAutocreate(true);
        // Then: no more exceptions
        if (!ff4j.check("does_not_exist")) {
            System.out.println("Auto created and set as false");
        }
    }

    @Test
    public void how_groups_works() {
        // Given: 2 features 'off' within existing group
        FF4j ff4j = new FF4j("ff4j.xml");
        Assertions.assertTrue(ff4j.exist("userStory3_1"));
        Assertions.assertTrue(ff4j.exist("userStory3_2"));
        //Assertions.assertTrue(ff4j.getStore().readAllGroups().contains("sprint_3"));
        Assertions.assertEquals("sprint_3", ff4j.getFeature("userStory3_1").getGroup());
        Assertions.assertEquals("sprint_3", ff4j.getFeature("userStory3_2").getGroup());
        Assertions.assertFalse(ff4j.check("userStory3_1"));
        Assertions.assertFalse(ff4j.check("userStory3_2"));

        // When: toggle group on
        //  ff4j.getStore().enableGroup("sprint_3");

        // Then: all features on
        Assertions.assertFalse(ff4j.check("userStory3_1"));
        Assertions.assertFalse(ff4j.check("userStory3_2"));
    }
}
