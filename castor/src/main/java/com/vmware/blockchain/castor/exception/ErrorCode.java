/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.exception;

/**
 * Error Messages. At some point we will turn this into a class loading from a resource bundle.
 */
@SuppressWarnings("LineLength")
public final class ErrorCode {
    public static final String BAD_GRPCS_CONFIGURATION = "Required property {0} for grpc over TLS is not set";
    public static final String INFRA_DESC_PROPERTY_NOT_SET = "Required infrastructure descriptor property {0} is not set";
    public static final String INFRA_DESC_FILE_MISSING = "Infrastructure descriptor file {0} is not available";
    public static final String INFRA_DESC_FILE_READ_ERROR = "Infrastructure descriptor file {0} could not be processed";
    public static final String DEPL_DESC_FILE_MISSING = "Deployment descriptor file {0} is not available";
    public static final String DEPL_DESC_FILE_READ_ERROR = "Deployment descriptor file {0} could not be processed";
    public static final String DEPL_DESC_PROPERTY_NOT_SET = "Required deployment descriptor property {0} is not set";
    public static final String PROVISIONING_ERROR = "Error in blockchain provisioning";
    public static final String DEPL_REQUEST_SUBMIT_ERROR = "Error in submitting a deployment request";
    public static final String INVALID_DESCRIPTOR_CONFIGURATION = "Could not find zone info in the infrastructure descriptor for zone: {}";
    public static final String DEPLOYMENT_ZONE_MISMATCH = "Specified deployment zones: {0} are not present in infrastructure descriptor";
    public static final String VALIDATION_ERRORS = "Infrastructure and/pr deployment descriptor specification has errors";
}
