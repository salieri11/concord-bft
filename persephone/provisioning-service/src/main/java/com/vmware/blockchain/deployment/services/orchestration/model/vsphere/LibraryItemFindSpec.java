/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.model.vsphere;

import lombok.Builder;
import lombok.Data;

/**
 * Data class.
 */
@Data
@Builder
public class LibraryItemFindSpec {
    String name;
    String libraryId;
    String sourceId;
    String type;
    Boolean cached;
}
