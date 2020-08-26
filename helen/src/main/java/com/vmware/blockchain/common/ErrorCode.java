/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

/**
 * Error Messages. At some point we will turn this into a class loading from a resource bundle.
 */
public final class ErrorCode {

    public static final String AGREEMENT_NOT_FOUND = "No agreement exists";
    public static final String BAD_GRPCS_CONFIGURATION = "Required property {0} for grpc over TLS is not set";
    public static final String BAD_LOGIN_REQUEST = "Invalid email/password";
    public static final String BAD_PASSWORD_CHANGE = "Can't use same password!";
    public static final String BAD_REQUEST = "Bad request (e.g. missing request body).";
    public static final String BAD_NUMBER_FORMAT = "Bad number format {0}";
    public static final String BAD_ORG_REMOVE = "Cannot remove the Consortium Owner Org";
    public static final String BAD_TOKEN = "Bad token";
    public static final String BAD_UUID_FORMAT = "Error converting {0} to UUID";
    public static final String BLOCKCHAIN_UNSPECIFIED = "No blockchain specified";
    public static final String BLOCKCHAIN_NOT_FOUND = "Blockchain with ID {0} not found.";
    public static final String BYTECODE_OR_METADATA_ALLOWED = "Only bytecode or metadata file path are supported";
    public static final String CANNOT_EXCHANGE_TOKEN = "Cannot exchange access code";
    public static final String CANNOT_REDIRECT_TO_TARGET = "Cannot redirect to {0}";
    public static final String CLIENT_NOT_FOUND = "Client with ID {0} not found on blockchain with ID {1}.";
    public static final String CONCORD_CONNECTION = "Unable to get concord connection";
    public static final String CONCORD_INVALID_RESPONSE = "Concord sent invalid response";
    public static final String CONCORD_INTERNAL_ERROR = "Concord internal error: ";
    public static final String CONCORD_SEND_FAILED = "concord send failed";
    public static final String CONCURRENT_UPDATE = "Concurrent update on {0}, column name {1}. Try operation again.";
    public static final String CONCURRENT_UPDATE_FAILED =
            "Concurrent update on {0}, column name {1}. Try operation again";
    public static final String CONNECTION_POOL_UNSUPPORTED = "getConnection, pool not initialized";
    public static final String CONSORTIUM_NOT_FOUND = "Consortium with ID {0} not found.";
    public static final String CONTRACT_COMPILE_FAILED = "Compilation failure {0}";
    public static final String CONTRACT_DEPLOY_FAILED = "Could not deploy contract {0}";
    public static final String CONTRACT_NOT_FOUND = "Contract not found: {0}";
    public static final String CONTRACT_NOT_OWNER = "Only original owner can deploy the new version of a contract";
    public static final String CONTRACT_VERSION_NOT_FOUND = "Contract version not found  {0}:{1}";
    public static final String CSP_INVALID_JWT_KEY_ID = "Invalid Key ID {0}";
    public static final String DATABASE_UNAVAILABLE = "Database service is not available";
    public static final String DELETE_INTEGRITY_ERROR =
            "Cannot delete entity with id {0} and column name {1}. It is referenced by other entities.";

    public static final String DE_REGISTERED = "Deregistered blockchains are hidden";
    public static final String DUPLICATE_CONTRACT_ID = "ContractVersion with id {0} and version {1} already exists";
    public static final String DUPLICATE_EMAIL = "Duplicate email address";
    public static final String DUPLICATE_UPDATION = "The row has already been updated";
    public static final String ENTITY_NOT_FOUND = "Entity is not found for Id {0}";
    public static final String ELEMENTS_SPECIFIED_MORE = "'params' must contain only one element";
    public static final String ENTITY_ISSUES = "Issues with entity";
    public static final String ENTITY_CONVERSION_UNSUCCESSFUL = "Could not convert from DB to Entity {0}";
    public static final String ELEMENTS_SPECIFIED_LESS = "Params should contain 2 elements for this request type";
    public static final String EXCEPTION_NOT_ALLOWED = "Not allowed";
    public static final String EXPECTED_AZPS_NULL_OR_EMPTY = "Expected list of Athentication Providers is missing";
    public static final String FAILED_TO_PARSE_DATE =
            "Failed to parse a date: {0}. Date format: {1}. Error message: {2}.";
    public static final String FROM_UNSPECIFIED = "'from' must be specified";
    public static final String HEX_COUNT_WRONG = "Hex string has odd nibble count.";
    public static final String ID_TYPE_WRONG = "id must be a number";
    public static final String ID_UNSPECIFIED = "request must contain an id";
    public static final String INVALID_BLOCK_NUMBER =
            "Invalid block number requested. Block number can either be 'latest', "
            + "'pending', 'earliest'or a hex number starting with '0x'";
    public static final String INVALID_BLOCK_REQUEST =
            "Invalid request: Choose correct block numbers, range or a block hash";
    public static final String INVALID_EMAIL = "invalid email specified";
    public static final String INTERNAL_ERROR = "An internal error has occurred. Please retry again later";
    public static final String INVALID_HEXCHAR = "Invalid hex character";
    public static final String INVALID_INPUT_SIZE = "Input is too large to put in byte array of size ";
    public static final String INVALID_INVITATION = "Invalid Invitation Link";
    public static final String INVALID_JWT_TOKEN = "Expired or invalid JWT token";
    public static final String INVALID_METHOD_NAME = "Invalid method name";
    public static final String INVALID_METHOD_TYPE = "method must be a string";
    public static final String INVALID_NAME = "invalid name specified";
    public static final String INVALID_NODE = "Invalid Node {0}";
    public static final String INVALID_PARAMETER = "'invalid param";
    public static final String INVALID_PASSWORD = "invalid password specified";
    public static final String INVALID_RAW_TRANSACTION = "Invalid raw transaction (signature R too large)";
    public static final String INVALID_REQUEST = "Invalid request";
    public static final String INVALID_ROLE = "{0} is invalid Role value.";
    public static final String INVALID_VALUE_SIZE = "Value too large for long";
    public static final String INVALID_ROLE_VALUE = "Invalid role value: ";
    public static final String INVALID_VERSION_PARAM_VAL =
            "start/end & max parameters to query versions of the Entity must be between 1 and {0}.";
    public static final String JSON_METHOD_UNSUPPORTED = "parseToJSON method is not supported";
    public static final String METHOD_UNSPECIFIED = "request must contain a method";
    public static final String NULL_TENANT_ID = "Null tenantId for column %s";
    public static final String NO_AUTHORIZATION = "No Authorization";
    public static final String NO_REPLICAS_FOUND = "No replicas found for blockchain with ID {0}.";
    public static final String NOT_ALLOWED = "Not allowed";
    public static final String NOT_FOUND = "Not found";
    public static final String ORG_NOT_FOUND = "Organization with ID {0} not found.";
    public static final String PASSPHRASE_INVALID = "Invalid passphrase";
    public static final String RAW_TRANSCATION_UNPARSED = "Unable to parse raw transaction (extra data after envelope)";
    public static final String REQUEST_UNPARSED = "unable to parse request";
    public static final String REPLICA_NOT_FOUND = "Replica with ID {0} not found on blockchain with ID {1}.";
    public static final String RETRY_FAILURE = "Failed to retry for {0}. Max retries exceeded.";
    public static final String UNALLOWED = "Not allowed or forbidden";
    public static final String UNINITIALIZED_POOL = "returnConnection, pool not initialized";
    public static final String UNMATCHED_QUERY = "Could not match query {0}";
    public static final String UNSUPPORTED_TYPE = "Type not supported {0}";
    public static final String USER_ID_NOT_FOUND = "No user found with ID {0}";
    public static final String USER_NOT_AUTHENTICATED = "User {0} is not authenticated";
    public static final String USER_NOT_FOUND = "No user with email {0}";
    public static final String UNKNOWN_FILTER = "Unknown filter type ";
    public static final String UNSUITABLE_PARAMETER_NUMBERS = "Too many parameters. Either none or a JSON object.";
    public static final String UUID_BINDING_UNSUCCESSFUL = "Failed to cast linked entity field to UUID.";
    public static final String TO_UNSPECIFIED = "'to' must be specified";
    public static final String ZONE_NOT_FOUND = "Zone with ID {0} not found.";



}
