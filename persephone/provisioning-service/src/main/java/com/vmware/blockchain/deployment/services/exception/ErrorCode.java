/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.exception;

/**
 * Error Messages.
 */
public final class ErrorCode {

    public static final String FOLDER_NOT_FOUND = "Error retrieving folder: {0}";
    public static final String GRPC_SSL_INIT_ERROR =
                                                "Error importing self-signed certificates for gRPC communication. {0}";
    public static final String INVALID_SESSION_ID = "Invalid session id: {0}";
    public static final String KEYSTORE_CREATION_ERROR = "Error in creating KeyStore with the provided certificate";
    public static final String NO_RESPONSE_RETRY = "No response after {0} retries";
    public static final String NOT_FOUND_DATASTORE = "Error retrieving datastore: {0}";
    public static final String NOT_FOUND_LIBRARY_ITEM = "Content Library not found with id {0}";
    public static final String NOT_FOUND_NETWORK = "Error retrieving network: {0}";
    public static final String NOT_FOUND_RESOURCE_POOL = "Error retrieving resource pool: {0}";
    public static final String NOT_FOUND_VM_INFO = "Unable to get VM info: {0}";
    public static final String NOTARY_SERVER_ADDRESS_MALFORMED = "Notary Server Address is malformed: {0}";
    public static final String CONTAINER_REG_ADDRESS_MALFORMED = "Container Registry Address is malformed: {0}";
    public static final String OPERATION_TIMEOUT = "Failed to complete the operation in time";
    public static final String RESOURCE_CREATION_FAILED = "Failed to create resource";
    public static final String RESOURCE_DELETION_FAILED = "Failed to delete resource";
    public static final String RESOURCE_NETWORK_DELETION_FAILURE = "Failed to delete network address {0}";
    public static final String REQUEST_EXECUTION_FAILURE = "Request execution failed";
    public static final String SITE_DATASTORE_INCORRECT = "Incorrect datastore information: {0}";
    public static final String SITE_FOLDER_INCORRECT = "Incorrect folder information: {0}";
    public static final String SITE_GATEWAY_IP_INCORRECT = "Provided gateway {0} not a valid Inet address";
    public static final String SITE_NETWORK_INCORRECT = "Incorrect network information: {0}";
    public static final String SITE_RESOURCE_POOL_INCORRECT = "Incorrect resource pool information: {0}";
    public static final String SSL_CONTEXT_CREATION_ERROR = "Error creating SSLContext";
    public static final String UNKNOWN_GATEWAY = "Unknown host exception while getting gateway for {0}";
    public static final String VM_CREATE_ERROR = "Error creating VM";
    public static final String VM_CPU_UPGRADE_ERROR = "Error upgrading VM CPU: {0}";
    public static final String VM_DISK_CREATE_ERROR = "Error creating VM disk: {0}";
    public static final String VM_MEMORY_UPGRADE_ERROR = "Error upgrading VM memory: {0}";
    public static final String VM_POWER_OFF_ERROR = "Unable to power off VM: {0}";
    public static final String VM_POWER_STATE_UPDATE_ERROR = "Unable to update VM power state: {0}";
    public static final String VM_START_ERROR = "Error creating/starting the VM";
    public static final String VMC_ORCHESTRATION_CREATION_FAILURE = "Error creating VMC orchestrator: {0}";
    public static final String SERVER_NOT_FUNCTIONAL = "Server is not reachable or is unhealthy: {0}";
    public static final String INVALID_CREDENTIALS = "Access credentials are invalid: {0}";
}
