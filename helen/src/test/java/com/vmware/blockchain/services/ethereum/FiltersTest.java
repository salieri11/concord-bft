/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.ethereum;

import java.util.Random;

import org.junit.Assert;
import org.junit.Test;

/**
 * Simple tests of FilterManager and EthereumFilter.
 */
public class FiltersTest {

    /**
     * A simple test to make sure initial values are recorded.
     */
    @Test
    public void directNewBlockFilterInitialValues() {
        Random rand = new Random();
        final String testId = Long.toHexString(rand.nextLong());
        final long testLatestBlock = rand.nextLong();

        final EthereumFilter testFilter = EthereumFilter.newBlockFilter(testId, testLatestBlock);

        Assert.assertEquals(EthereumFilter.FilterType.BlockFilter, testFilter.type);
        Assert.assertEquals(testId, testFilter.id);
        Assert.assertEquals(testLatestBlock, testFilter.latestBlock);
        Assert.assertEquals(true, testFilter.delay);
    }

    /**
     * A simple tests to make sure update values are recorded, and non-chaning fields don't change.
     */
    @Test
    public void directUpdateBlockFilterValues() {
        Random rand = new Random();
        final String testId = Long.toHexString(rand.nextLong());
        final long testInitialBlock = rand.nextLong();
        final long testLatestBlock = rand.nextLong();

        final EthereumFilter initialFilter = EthereumFilter.newBlockFilter(testId, testInitialBlock);
        final EthereumFilter updatedFilter = initialFilter.updateFilter(testLatestBlock);

        // type and ID should not change
        Assert.assertEquals(EthereumFilter.FilterType.BlockFilter, updatedFilter.type);
        Assert.assertEquals(testId, updatedFilter.id);

        // latest block should be new value
        Assert.assertEquals(testLatestBlock, updatedFilter.latestBlock);

        // and delay should clear
        Assert.assertEquals(false, updatedFilter.delay);
    }

    /**
     * Repeat the initial and update tests above, but go through the FilterManager.
     */
    @Test
    public void managerBlockFilterValues() {
        Random rand = new Random();
        final long testInitialBlock = rand.nextLong();
        final long testLatestBlock = rand.nextLong();

        final String initialFilterId = FilterManager.createBlockFilter(testInitialBlock);
        final EthereumFilter initialFilter = FilterManager.getFilter(initialFilterId);

        Assert.assertEquals(initialFilterId, initialFilter.id);
        Assert.assertEquals(EthereumFilter.FilterType.BlockFilter, initialFilter.type);
        Assert.assertEquals(testInitialBlock, initialFilter.latestBlock);
        Assert.assertEquals(true, initialFilter.delay);

        FilterManager.updateFilter(initialFilter, testLatestBlock);
        final EthereumFilter updatedFilter = FilterManager.getFilter(initialFilter.id);

        // this "not equal" is a little specious, but EthereumFilter is documented as immutable, so this a prevention of
        // a change to that guarantee
        Assert.assertNotEquals(initialFilter, updatedFilter);
        Assert.assertEquals(initialFilterId, updatedFilter.id);
        Assert.assertEquals(EthereumFilter.FilterType.BlockFilter, updatedFilter.type);
        Assert.assertEquals(testLatestBlock, updatedFilter.latestBlock);
        Assert.assertEquals(false, updatedFilter.delay);
    }

    /**
     * Test that new IDs are created for each filter.
     */
    @Test
    public void managerCreatesNewIds() {
        final int testFilterCount = 10;
        String[] filterIds = new String[testFilterCount];

        for (int i = 0; i < testFilterCount; i++) {
            // the latest block parameter doens't matter here
            filterIds[i] = FilterManager.createBlockFilter(0);

            // this new filter's ID shouldn't be the same as any already created
            for (int j = 0; j < i; j++) {
                Assert.assertNotEquals(filterIds[j], filterIds[i]);
            }
        }
    }

    /**
     * Test that uninstalling a filter makes it unfindable.
     */
    @Test
    public void managerUninstallsFilters() {
        // latest block parameter doesn't matter here
        final String filterId = FilterManager.createBlockFilter(0);

        boolean uninstallSuccess = FilterManager.uninstallFilter(filterId);

        Assert.assertEquals(true, uninstallSuccess);

        final EthereumFilter foundFilter = FilterManager.getFilter(filterId);

        Assert.assertEquals(null, foundFilter);
    }
}
