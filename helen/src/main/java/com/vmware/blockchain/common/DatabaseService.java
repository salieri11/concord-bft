/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import java.sql.Connection;
import java.sql.DriverManager;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * A class which manages and provides access to database Connection objects.
 */
@Component
public class DatabaseService {

    // TODO: create a pool of connection objects rather than using just a single
    // object
    private static Connection db;
    private static boolean initDone;
    private static Logger logger = LogManager.getLogger(DatabaseService.class);

    private String dbProtocol;
    private String dbIp;
    private String dbPort;
    private String dbName;
    private String dbOptions;
    private String dbUser;
    private String dbPassword;

    @Autowired
    public DatabaseService(@Value("${DB_PROTOCOL}") String dbProtocol, @Value("${DB_IP}") String dbIp,
            @Value("${DB_PORT}") String dbPort, @Value("${DB_NAME}") String dbName,
            @Value("${DB_OPTIONS}") String dbOptions, @Value("${DB_USER}") String dbUser,
            @Value("${DB_PASSWORD}") String dbPassword) {
        this.dbProtocol = dbProtocol;
        this.dbIp = dbIp;
        this.dbPort = dbPort;
        this.dbName = dbName;
        this.dbOptions = dbOptions;
        this.dbUser = dbUser;
        this.dbPassword = dbPassword;
    }

    private void init() throws Exception {
        String url = String.format("%s://%s:%s/%s?%s", dbProtocol, dbIp, dbPort, dbName, dbOptions);
        logger.debug("Connecting to database at: " + url);
        db = DriverManager.getConnection(url, dbUser, dbPassword);
    }

    /**
     * Get a DB Connection.
     *
     * @return Connection
     */
    public synchronized Connection getDatabaseConnection() throws ServiceUnavailableException {
        if (!initDone) {
            try {
                init();
            } catch (Exception e) {
                logger.error(e);
            }
            initDone = true;
        }

        if (initDone && db == null) {
            throw new ServiceUnavailableException(ErrorCodeType.DATABASE_UNAVAILABLE);
        }

        return db;
    }
}
