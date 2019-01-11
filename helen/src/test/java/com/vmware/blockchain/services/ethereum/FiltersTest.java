/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.ethereum;

import java.util.Random;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Simple tests of FilterManager and EthereumFilter.
 */
class FiltersTest {

    /**
     * A simple test to make sure initial values are recorded.
     */
    @Test
    void directNewBlockFilterInitialValues() {
        Random rand = new Random();
        final String testId = Long.toHexString(rand.nextLong());
        final long testLatestBlock = rand.nextLong();

        final EthereumFilter testFilter = EthereumFilter.newBlockFilter(testId, testLatestBlock);

        Assertions.assertEquals(EthereumFilter.FilterType.BlockFilter, testFilter.type);
        Assertions.assertEquals(testId, testFilter.id);
        Assertions.assertEquals(testLatestBlock, testFilter.latestBlock);
        Assertions.assertTrue(testFilter.delay);
    }

    /**
     * A simple tests to make sure update values are recorded, and non-chaning fields don't change.
     */
    @Test
    void directUpdateBlockFilterValues() {
        Random rand = new Random();
        final String testId = Long.toHexString(rand.nextLong());
        final long testInitialBlock = rand.nextLong();
        final long testLatestBlock = rand.nextLong();

        final EthereumFilter initialFilter = EthereumFilter.newBlockFilter(testId, testInitialBlock);
        final EthereumFilter updatedFilter = initialFilter.updateFilter(testLatestBlock);

        // type and ID should not change
        Assertions.assertEquals(EthereumFilter.FilterType.BlockFilter, updatedFilter.type);
        Assertions.assertEquals(testId, updatedFilter.id);

        // latest block should be new value
        Assertions.assertEquals(testLatestBlock, updatedFilter.latestBlock);

        // and delay should clear
        Assertions.assertFalse(updatedFilter.delay);
    }

    /**
     * Repeat the initial and update tests above, but go through the FilterManager.
     */
    @Test
    void managerBlockFilterValues() {
        Random rand = new Random();
        final long testInitialBlock = rand.nextLong();
        final long testLatestBlock = rand.nextLong();

        final String initialFilterId = FilterManager.createBlockFilter(testInitialBlock);
        final EthereumFilter initialFilter = FilterManager.getFilter(initialFilterId);

        Assertions.assertEquals(initialFilterId, initialFilter.id);
        Assertions.assertEquals(EthereumFilter.FilterType.BlockFilter, initialFilter.type);
        Assertions.assertEquals(testInitialBlock, initialFilter.latestBlock);
        Assertions.assertTrue(initialFilter.delay);

        FilterManager.updateFilter(initialFilter, testLatestBlock);
        final EthereumFilter updatedFilter = FilterManager.getFilter(initialFilter.id);

        // this "not equal" is a little specious, but EthereumFilter is documented as immutable, so this a prevention of
        // a change to that guarantee
        Assertions.assertNotEquals(initialFilter, updatedFilter);
        Assertions.assertEquals(initialFilterId, updatedFilter.id);
        Assertions.assertEquals(EthereumFilter.FilterType.BlockFilter, updatedFilter.type);
        Assertions.assertEquals(testLatestBlock, updatedFilter.latestBlock);
        Assertions.assertFalse(updatedFilter.delay);
    }

    /**
     * Test that new IDs are created for each filter.
     */
    @Test
    void managerCreatesNewIds() {
        final int testFilterCount = 10;
        String[] filterIds = new String[testFilterCount];

        for (int i = 0; i < testFilterCount; i++) {
            // the latest block parameter doens't matter here
            filterIds[i] = FilterManager.createBlockFilter(0);

            // this new filter's ID shouldn't be the same as any already created
            for (int j = 0; j < i; j++) {
                Assertions.assertNotEquals(filterIds[j], filterIds[i]);
            }
        }
    }

    /**
     * Test that uninstalling a filter makes it unfindable.
     */
    @Test
    void managerUninstallsFilters() {
        // latest block parameter doesn't matter here
        final String filterId = FilterManager.createBlockFilter(0);

        boolean uninstallSuccess = FilterManager.uninstallFilter(filterId);

        Assertions.assertTrue(uninstallSuccess);

        final EthereumFilter foundFilter = FilterManager.getFilter(filterId);

        Assertions.assertNull(foundFilter);
    }
}
