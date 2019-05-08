/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.awsutil;

import java.io.File;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.AWSStaticCredentialsProvider;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3ClientBuilder;
import com.amazonaws.services.s3.model.PutObjectResult;
import com.amazonaws.services.s3.model.S3Object;


/**
 *  Utility class for talking to AWS S3.
 */
public final class AwsS3Client {
    private final AmazonS3 s3client;

    /**
     * Constructor for connecting to S3.
     */
    public AwsS3Client(String awsAccessKey, String awsSecretKey, String regionName) {
        AWSCredentials credentials = new BasicAWSCredentials(awsAccessKey, awsSecretKey);
        Regions region;
        try {
            region = Regions.fromName(regionName);
        } catch (IllegalArgumentException e) {
            region = Regions.valueOf(regionName);
        }

        s3client = AmazonS3ClientBuilder.standard()
          .withCredentials(new AWSStaticCredentialsProvider(credentials))
          .withRegion(region)
          .build();
    }

    public AwsS3Client(AmazonS3 s3client) {
        this.s3client = s3client;
    }

    //uploading object
    public PutObjectResult putObject(String bucketName, String key, File file) {
        return s3client.putObject(bucketName, key, file);
    }

    // get an object
    public S3Object getObject(String bucketName, String objectKey) {
        return s3client.getObject(bucketName, objectKey);
    }
}
