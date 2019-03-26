/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.db;

import java.util.UUID;

import lombok.Getter;
import lombok.Setter;

/**
 * Represents a row in the link table.  This indicates a relation between the two entities.
 */
@Getter
@Setter
public class Link {
    UUID fromRow;
    UUID toRow;
}
