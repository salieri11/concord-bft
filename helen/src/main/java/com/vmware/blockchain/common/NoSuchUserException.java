package com.vmware.blockchain.common;

import org.springframework.http.HttpStatus;

public class NoSuchUserException extends HelenException {
    private static final long serialVersionUID = 1L;

    public NoSuchUserException(String message) {
        super(message, HttpStatus.BAD_REQUEST);
    }
}
