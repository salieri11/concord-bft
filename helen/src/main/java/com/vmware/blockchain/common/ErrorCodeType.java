/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import lombok.Value;
/**
 * Localizable ErrorCodeTypes in resource bundles.
 */

@Value
public final class ErrorCodeType {

    /**
     * Use generic to specific principle (i.e. world.country.state.county.city...).
     *
     * <p>All error code string should start with prefix helen.
     *
     * <p>For easier maintenance, please order alphabetically.
     */

    public static final ErrorCodeType AGREEMENT_NOT_FOUND = new ErrorCodeType("helen.no.agreement.exists", 0);
    public static final ErrorCodeType BAD_LOGIN_REQUEST = new ErrorCodeType("helen.invalid.email/password", 0);
    public static final ErrorCodeType BAD_PASSWORD_CHANGE = new ErrorCodeType("helen.cant.use.same.password", 0);
    public static final ErrorCodeType BAD_REQUEST = new ErrorCodeType("helen.bad.request.missing.request.body", 0);
    public static final ErrorCodeType BAD_REQUEST_PARAM = new ErrorCodeType("helen.bad.request.missing.request.param",
                                                                            0);
    public static final ErrorCodeType BAD_NUMBER_FORMAT = new ErrorCodeType("helen.bad.number.format", 1);
    public static final ErrorCodeType BAD_ORG_REMOVE = new ErrorCodeType("helen.cannot.remove.the.consortium.owner.org",
                                                                         0);
    public static final ErrorCodeType BAD_TOKEN = new ErrorCodeType("helen.bad.token", 0);
    public static final ErrorCodeType BAD_UUID_FORMAT = new ErrorCodeType("helen.error.converting.to.UUID", 1);
    public static final ErrorCodeType BLOCKCHAIN_UNSPECIFIED = new ErrorCodeType("helen.no.blockchain.specified", 0);
    public static final ErrorCodeType BYTECODE_OR_METADATA_ALLOWED = new ErrorCodeType("helen.only."
                                      + "metadata.file.path.are.supported", 0);
    public static final ErrorCodeType CANNOT_ACCESS_BLOCKCHAIN = new ErrorCodeType("helen.cannot.access.blockchain", 0);
    public static final ErrorCodeType CANNOT_EXCHANGE_TOKEN = new ErrorCodeType("helen.cannot.exchange.access.code",
                                                                                0);
    public static final ErrorCodeType CANNOT_REDIRECT_TO_TARGET = new ErrorCodeType("helen.cannot.redirect.to.target",
                                                                                    1);
    public static final ErrorCodeType CONCORD_CONNECTION = new ErrorCodeType("helen.unable.to.get.concord.connection",
                                                                             0);
    public static final ErrorCodeType CONCORD_INVALID_RESPONSE = new ErrorCodeType("helen.concord.sent.invalid."
                                                                                   + "response", 0);
    public static final ErrorCodeType CONCORD_INTERNAL_ERROR = new ErrorCodeType("helen.concord.internal.error", 0);
    public static final ErrorCodeType CONCORD_SEND_FAILED = new ErrorCodeType("helen.concord.send.failed", 0);
    public static final ErrorCodeType CONCURRENT_UPDATE = new ErrorCodeType("helen.concurrent.update.on.column", 2);
    public static final ErrorCodeType CONCURRENT_UPDATE_FAILED = new ErrorCodeType("helen.concurrent.update.failed", 2);
    public static final ErrorCodeType CONNECTION_POOL_UNSUPPORTED = new ErrorCodeType("helen.connection"
                                                                      + "pool.uninitialized", 0);
    public static final ErrorCodeType CONTRACT_COMPILE_FAILED = new ErrorCodeType("helen.compilation.failure", 1);
    public static final ErrorCodeType CONTRACT_DEPLOY_FAILED = new ErrorCodeType("helen.could.not.deploy.contract", 1);
    public static final ErrorCodeType CONTRACT_NOT_FOUND = new ErrorCodeType("helen.contract.not.found", 1);
    public static final ErrorCodeType CONTRACT_NOT_OWNER = new ErrorCodeType("helen.only.original.owner.can."
                                                           + "deploy.the.new.version.of.contract", 0);
    public static final ErrorCodeType CONTRACT_VERSION_NOT_FOUND = new ErrorCodeType("helen.contract.version."
                                                                       + "not.found", 2);
    public static final ErrorCodeType CSP_INVALID_JWT_KEY_ID = new ErrorCodeType("helen.invalid.key.ID", 0);
    public static final ErrorCodeType DATABASE_UNAVAILABLE = new ErrorCodeType("helen.database.service.is."
                                                             + "not.available", 0);
    public static final ErrorCodeType DELETE_INTEGRITY_ERROR = new ErrorCodeType("helen.cannot.delete."
                                                               + "entity.with.id.and.column.name.", 2);
    public static final ErrorCodeType DUPLICATE_CONTRACT_ID = new ErrorCodeType("helen.contract.version."
                                                              + "with.id.and.version.already.exists", 2);
    public static final ErrorCodeType DUPLICATE_EMAIL = new ErrorCodeType("helen.duplicate.email.address", 0);
    public static final ErrorCodeType DUPLICATE_UPDATION = new ErrorCodeType("helen.row.has.already.been.updated.", 0);
    public static final ErrorCodeType ENTITY_NOT_FOUND = new ErrorCodeType("helen.entity.is.not.found.for"
                                                          + ".Id.and.column.name", 2);
    public static final ErrorCodeType ELEMENTS_SPECIFIED_MORE = new ErrorCodeType("helen.params.must.contain."
                                                                                  + "only.one.element", 0);
    public static final ErrorCodeType ENTITY_ISSUES = new ErrorCodeType("helen.issues"
                                                       + ".with.entity", 0);
    public static final ErrorCodeType ENTITY_CONVERSION_UNSUCCESSFUL = new ErrorCodeType("helen.could.not."
                                                                        + "convert.from.db.to.entity", 1);
    public static final ErrorCodeType ELEMENTS_SPECIFIED_LESS = new ErrorCodeType("helen.params.should."
                                                               + "contains.two.elements.for.this.request.type", 0);
    public static final ErrorCodeType EXCEPTION_NOT_ALLOWED = new ErrorCodeType("helen.not.allowed", 0);
    public static final ErrorCodeType EXPECTED_AZPS_NULL_OR_EMPTY = new ErrorCodeType("helen.expected.list.of."
                                                                     + "authentication", 0);
    public static final ErrorCodeType FAILED_TO_PARSE_DATE = new ErrorCodeType("helen.failed.to.parse.date.format", 3);
    public static final ErrorCodeType FROM_UNSPECIFIED = new ErrorCodeType("helen.from.must.be.specified", 0);
    public static final ErrorCodeType HEX_COUNT_WRONG = new ErrorCodeType("helen.hex.string.has.odd.nibble.count", 0);
    public static final ErrorCodeType ID_TYPE_WRONG = new ErrorCodeType("helen.id.must.be.a.number", 0);
    public static final ErrorCodeType ID_UNSPECIFIED = new ErrorCodeType("helen.request.must.contain.an.id", 0);
    public static final ErrorCodeType INVALID_BLOCK_NUMBER = new ErrorCodeType("helen.invalid."
        + "block.number.requested.block.number.can.either.be.latest.pending.earliest.or.a.hex.number.starting."
                                                                               + "with.0x", 0);
    public static final ErrorCodeType INVALID_BLOCK_REQUEST =
            new ErrorCodeType("helen.invalid.request.choose.correct.block.numbers.range.or.a.block.hash", 0);
    public static final ErrorCodeType INVALID_EMAIL = new ErrorCodeType("helen.invalid.email.specified", 0);
    public static final ErrorCodeType INTERNAL_ERROR = new ErrorCodeType("helen.internal.error", 0);
    public static final ErrorCodeType INVALID_HEXCHAR = new ErrorCodeType("helen.invalid.hex.character", 0);
    public static final ErrorCodeType INVALID_INPUT_SIZE = new ErrorCodeType("helen.input.is.too.l"
                                     + "arge.to.put.in.byte.array.of.size", 0);
    public static final ErrorCodeType INVALID_INVITATION = new ErrorCodeType("helen.invalid.invitation.link", 0);
    public static final ErrorCodeType INVALID_JWT_TOKEN = new ErrorCodeType("helen.expired.or.invalid.JWT.token", 0);
    public static final ErrorCodeType INVALID_METHOD_NAME = new ErrorCodeType("helen.invalid.method.name", 0);
    public static final ErrorCodeType INVALID_METHOD_TYPE = new ErrorCodeType("helen.method.must.be.a.string", 0);
    public static final ErrorCodeType INVALID_NAME = new ErrorCodeType("helen.invalid.name.specified", 0);
    public static final ErrorCodeType INVALID_NODE = new ErrorCodeType("helen.invalid.node", 1);
    public static final ErrorCodeType INVALID_PARAMETER = new ErrorCodeType("helen.invalid.param", 0);
    public static final ErrorCodeType INVALID_PASSWORD = new ErrorCodeType("helen.invalid.password.specified", 0);
    public static final ErrorCodeType INVALID_RAW_TRANSACTION = new ErrorCodeType("helen.invalid."
                                       + "raw.transaction", 0);
    public static final ErrorCodeType INVALID_REQUEST = new ErrorCodeType("helen.invalid.request", 0);
    public static final ErrorCodeType INVALID_ROLE = new ErrorCodeType("helen.invalid.role.value.", 1);
    public static final ErrorCodeType INVALID_VALUE_SIZE = new ErrorCodeType("helen.value.too.large.for.long", 0);
    public static final ErrorCodeType INVALID_ROLE_VALUE = new ErrorCodeType("helen.invalid.role.value", 0);
    public static final ErrorCodeType INVALID_VERSION_PARAM_VAL =
            new ErrorCodeType("helen.invalid.version.parameter.value", 1);
    public static final ErrorCodeType JSON_METHOD_UNSUPPORTED = new ErrorCodeType("helen.parseToJSON."
                                      + "method.is.not.supported", 0);
    public static final ErrorCodeType METHOD_UNSPECIFIED = new ErrorCodeType("helen.request.must.contain.a.method", 0);
    public static final ErrorCodeType NULL_TENANT_ID = new ErrorCodeType("helen.null.tenantId.for.column.%s", 0);
    public static final ErrorCodeType NO_AUTHORIZATION = new ErrorCodeType("helen.no.authorization", 0);
    public static final ErrorCodeType NOT_ALLOWED = new ErrorCodeType("helen.not.allowed", 0);
    public static final ErrorCodeType NOT_FOUND = new ErrorCodeType("helen.not.found", 0);
    public static final ErrorCodeType ORG_NOT_FOUND = new ErrorCodeType("helen.Organization with ID {0} not found.", 1);
    public static final ErrorCodeType PASSPHRASE_INVALID = new ErrorCodeType("helen.invalid.passphrase.", 0);
    public static final ErrorCodeType RAW_TRANSCATION_UNPARSED = new ErrorCodeType("helen.unable."
                                      + "to.parse.raw.transaction", 0);
    public static final ErrorCodeType REQUEST_UNPARSED = new ErrorCodeType("helen.unable.to.parse.request.", 0);
    public static final ErrorCodeType RETRY_FAILURE = new ErrorCodeType("helen.failed.to.retry", 1);
    public static final ErrorCodeType UNALLOWED = new ErrorCodeType("helen.not.allowed.or.forbidden", 0);
    public static final ErrorCodeType UNINITIALIZED_POOL = new ErrorCodeType("helen.connection.pool.uninitialized", 0);
    public static final ErrorCodeType UNMATCHED_QUERY = new ErrorCodeType("helen.could.not.match.query", 1);
    public static final ErrorCodeType UNSUPPORTED_TYPE = new ErrorCodeType("helen.type.not.supported", 1);
    public static final ErrorCodeType USER_ID_NOT_FOUND = new ErrorCodeType("helen.no.user.found.with.ID", 1);
    public static final ErrorCodeType USER_NOT_AUTHENTICATED = new ErrorCodeType("helen.user.is.not.authenticated", 1);
    public static final ErrorCodeType USER_NOT_FOUND = new ErrorCodeType("helen.no.user.with.email", 1);
    public static final ErrorCodeType UNKNOWN_FILTER = new ErrorCodeType("helen.unknown.filter.type", 0);
    public static final ErrorCodeType UNSUITABLE_PARAMETER_NUMBERS = new ErrorCodeType("helen.too.many."
                                       + "parameters.either.none.or.a.JSON.object", 0);
    public static final ErrorCodeType UUID_BINDING_UNSUCCESSFUL = new ErrorCodeType("helen.failed.to."
                                       + "cast.linked.entity.field.to.UUID.", 0);
    public static final ErrorCodeType TO_UNSPECIFIED = new ErrorCodeType("helen.to.must.be.specified", 0);

    private final String errorCodeTypeValue;

    /**
     * This field contains the number of param that is required in the given ErrorCodeType to form a proper message.
     * It will be used in future to enforce compile-time check that correct set of arguments
     * are passed for a given error code.
     */
    private final int numOfParams;

}
