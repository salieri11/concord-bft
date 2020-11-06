/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.util;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * Blockchain ReadReplica class.
 */
@Getter
@Setter
@ToString
public class BlockchainReadReplica extends BlockchainNode {
    public static final String OBJ_STORE_ACCESS_KEY = "s3-access-key";
    public static final String OBJ_STORE_BUCKET_NAME_KEY = "s3-bucket-name";
    public static final String OBJ_STORE_PROTOCOL_KEY = "s3-protocol";
    public static final String OBJ_STORE_SECRET_KEY = "s3-secret-key";
    public static final String OBJ_STORE_URL_KEY = "s3-url";

    private String objStoreBucketName;
    private String objStoreAccessKey;
    private String objStoreProtocol;
    private String objStoreUrl;
    private String objStoreSecret;

    /**
     * Read replica constructor.
     * @param id node id
     * @param ip node ip
     */
    public BlockchainReadReplica(String id, String ip) {
        super(id, ip);
    }
}
