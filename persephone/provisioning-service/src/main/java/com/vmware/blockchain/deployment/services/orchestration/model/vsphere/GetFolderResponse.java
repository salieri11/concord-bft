/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.model.vsphere;

import java.util.List;

import lombok.Data;

/**
 * Data class for Folder response.
 */
@Data
public class GetFolderResponse {
    List<FolderSummary> value;
}
