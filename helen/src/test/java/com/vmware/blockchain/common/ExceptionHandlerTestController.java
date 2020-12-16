/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import java.io.IOException;
import java.util.UUID;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.google.protobuf.InvalidProtocolBufferException;

/**
 * Controller that throws various exceptions.  Used to test error handling.
 */
@RestController
public class ExceptionHandlerTestController {

    @RequestMapping(path = "/api/badrequest", method = RequestMethod.GET)
    ResponseEntity<String> badRequest() {
        throw new BadRequestException("This is your bad request");
    }

    @RequestMapping(path = "/api/concordexception", method = RequestMethod.GET)
    ResponseEntity<String> concordException() {
        throw new ConcordConnectionException("Connection error");
    }

    @RequestMapping(path = "/api/conflict", method = RequestMethod.GET)
    ResponseEntity<String> conflict() {
        throw new ConflictException("This is your conflict");
    }

    @RequestMapping(path = "/api/entitymodification", method = RequestMethod.GET)
    ResponseEntity<String> entityModification() {
        throw new EntityModificationException(ErrorCodeType.ENTITY_ISSUES);
    }

    @RequestMapping(path = "/api/forbidden", method = RequestMethod.GET)
    ResponseEntity<String> forbidden() {
        throw new ForbiddenException("No access");
    }

    @RequestMapping(path = "/api/notfound", method = RequestMethod.GET)
    ResponseEntity<String> notFound() {
        throw new NotFoundException("Nothing here to see");
    }

    @RequestMapping(path = "/api/serviceunavailable", method = RequestMethod.GET)
    ResponseEntity<String> serviceUnavailable() {
        throw new ServiceUnavailableException("System not ready yet");
    }

    @RequestMapping(path = "/api/unauthorized", method = RequestMethod.GET)
    ResponseEntity<String> unauthorized() {
        throw new UnauthorizedException("No authentication");
    }

    @RequestMapping(path = "/api/wallet", method = RequestMethod.GET)
    ResponseEntity<String> wallet() {
        throw new WalletException("Wallet problem");
    }

    @RequestMapping(path = "/api/unsupported", method = RequestMethod.GET)
    ResponseEntity<String> unsupported() {
        throw new UnsupportedOperationException("unsupported operation");
    }

    @RequestMapping(path = "/api/illegalarg", method = RequestMethod.GET)
    ResponseEntity<String> illegalArg() {
        throw new IllegalArgumentException("Illegal Argument");
    }

    @RequestMapping(path = "/api/io", method = RequestMethod.GET)
    ResponseEntity<String> io() throws Exception {
        throw new IOException("IO Failure");
    }

    @RequestMapping(path = "/api/protocol", method = RequestMethod.GET)
    ResponseEntity<String> protocol() throws Exception {
        throw new InvalidProtocolBufferException("Protobuf problem");
    }

    @RequestMapping(path = "/api/denied", method = RequestMethod.GET)
    ResponseEntity<String> denied() {
        throw new AccessDeniedException("Access denied");
    }

    // test the multi argument
    @RequestMapping(path = "/api/text", method = RequestMethod.GET)
    ResponseEntity<String> textMessage(@RequestParam String arg1, @RequestParam String arg2) {
        throw new BadRequestException("Bad request: {0}, {1}", arg1, arg2);
    }

    // test that a bad UUID parameter shows up as a 400
    @RequestMapping(path = "/api/uuid/{uuid}", method = RequestMethod.GET)
    ResponseEntity<String> testUuid(@PathVariable UUID uuid) {
        return new ResponseEntity<>("Success", HttpStatus.OK);
    }

    // Finally, test an exception that isn't specifically handled.  This should default to 500.
    @RequestMapping(path = "/api/random", method = RequestMethod.GET)
    ResponseEntity<String> randomError() {
        int i = 1;
        int j = i - 1;
        String message = Integer.toString(i / j);
        return new ResponseEntity<>(message, HttpStatus.OK);
    }
}
