/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.util.List;

import lombok.Builder;
import lombok.Getter;

/**
 * Container for validation error messages.
 */
@Getter
@Builder
public class ValidationError {
    private final String errorCode;
    private final String propertyPath;
    private final List<String> arguments;
}
