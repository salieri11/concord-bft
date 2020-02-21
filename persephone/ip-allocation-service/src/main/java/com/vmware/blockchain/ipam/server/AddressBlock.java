/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.ipam.server;

import com.vmware.blockchain.dao.AbstractEntity;
import com.vmware.blockchain.dao.EntityColumnName;

/**
 * Class representing Address Blocks.
 */
@EntityColumnName("persephone.addressblock")
public class AddressBlock extends AbstractEntity {
    String name;
    State state;
    BlockSpecification specification;

    /**
     * Constructor used for default AddressBlock values.
     */
    public AddressBlock() {
        this.name = "";
        this.specification = new BlockSpecification();
        this.state = State.ACTIVE;
    }

    /**
     * All args constructor used for AddressBlock values.
     */
    public AddressBlock(String name, BlockSpecification blockSpecification, State state) {
        this.name = name;
        this.specification = blockSpecification;
        this.state = state;
    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public BlockSpecification getSpecification() {
        return this.specification;
    }

    public void setSpecification(BlockSpecification specification) {
        this.specification = specification;
    }

    public State getState() {
        return this.state;
    }

    public void setState(State state) {
        this.state = state;
    }

    /**
     * Enum to determine state.
     */
    public static enum State {
        ACTIVE,
        CREATING,
        DELETING
    }
}
