/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.ipam.services.dao;

import java.util.UUID;

import javax.annotation.Nullable;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.dao.GenericDao;

/**
 * DAO for IPAllocation objects.
 */
@Component
public class IpAllocationDao {
    private GenericDao genericDao;

    @Autowired
    IpAllocationDao(GenericDao genericDao) {
        this.genericDao = genericDao;
    }

    /**
     * Gets the AddressBlock for given blockId.
     * blockId is the ResourceName for the given AddressBlock converted to a UUID.
     *
     * @param blockId the Block ID
     * @return the AddressBlock
     */
    @Nullable
    public AddressBlock getAddressBlock(UUID blockId) {
        AddressBlock addressBlock;
        try {
            addressBlock = genericDao.get(blockId, AddressBlock.class);
        } catch (NotFoundException e) {
            addressBlock = null;
        }
        return addressBlock;
    }

    /**
     * Creates an AddressBlock.
     *
     * @param addressBlock the AddressBlock
     * @return the AddressBlock created
     */
    public AddressBlock createAddressBlock(AddressBlock addressBlock) {
        return genericDao.put(addressBlock, null);
    }

    /**
     * Updates an AddressBlock.
     *
     * @param addressBlock the AddressBlock
     * @return the address block created
     */
    public AddressBlock updateAddressBlock(AddressBlock addressBlock) {
        return genericDao.put(addressBlock, addressBlock);
    }

    /**
     * Deletes an AddressBlock.
     *
     * @param blockId the AddressBlock ID
     */
    public void deleteAddressBlock(UUID blockId) throws NotFoundException {
        genericDao.delete(blockId, AddressBlock.class);
    }

    /**
     * Gets the AddressBlockSegment for given segmentId.
     * blockId is the ResourceName for the given AddressBlockSegment converted to a UUID.
     *
     * @param segmentId the AddressBlockSegment ID
     * @return the AddressBlockSegment
     */
    public AddressBlockSegment getAddressBlockSegment(UUID segmentId) throws NotFoundException {
        return genericDao.get(segmentId, AddressBlockSegment.class);
    }

    /**
     * Creates an AddressBlockSegment.
     *
     * @param addressBlockSegment the AddressBlockSegment
     * @return the address block segment created
     */
    public AddressBlockSegment createAddressBlockSegment(AddressBlockSegment addressBlockSegment) {
        return genericDao.put(addressBlockSegment, null);
    }

    /**
     * Updates an AddressBlockSegment.
     *
     * @param addressBlockSegment the AddressBlockSegment
     * @return the AddressBlockSegment created
     */
    public AddressBlockSegment updateAddressBlockSegment(AddressBlockSegment addressBlockSegment) {
        return genericDao.put(addressBlockSegment, addressBlockSegment);
    }

    /**
     * Deletes an AddressBlockSegment.
     *
     * @param segmentId the AddressBlockSegment ID
     */
    public void deleteAddressBlockSegment(UUID segmentId) throws NotFoundException {
        genericDao.delete(segmentId, AddressBlockSegment.class);
    }

}