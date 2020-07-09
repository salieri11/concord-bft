/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.models;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Class for Node credentials response.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class NodeGetCredentialsResponse {
    // TODO: username is a placeholder if it ever needs to be non-root.
    public String username;
    public String password;
}
