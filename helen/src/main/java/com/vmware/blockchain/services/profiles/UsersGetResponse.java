/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import org.json.simple.JSONObject;

/**
 * A interface which defines the API for response generated for a GET request in User management API.
 */
public interface UsersGetResponse {
    JSONObject toJson();
}
