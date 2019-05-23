/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.awsutil;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.X509EncodedKeySpec;

import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.AWSStaticCredentialsProvider;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3ClientBuilder;
import com.amazonaws.services.s3.AmazonS3EncryptionClientBuilder;
import com.amazonaws.services.s3.model.EncryptionMaterials;
import com.amazonaws.services.s3.model.PutObjectResult;
import com.amazonaws.services.s3.model.S3Object;
import com.amazonaws.services.s3.model.StaticEncryptionMaterialsProvider;


/**
 *  Utility class for talking to AWS S3.
 */
public final class AwsS3Client {
    private final AmazonS3 s3client;
    private String masterKey;
    private boolean encryptionEnabled;

    /**
     * Constructor for connecting to S3.
     */
    public AwsS3Client(String awsAccessKey, String awsSecretKey, String regionName) {
        encryptionEnabled = false;
        Regions region;
        try {
            region = Regions.fromName(regionName);
        } catch (IllegalArgumentException e) {
            region = Regions.valueOf(regionName);
        }

        AWSCredentials credentials = new BasicAWSCredentials(awsAccessKey, awsSecretKey);
        s3client = AmazonS3ClientBuilder.standard()
          .withCredentials(new AWSStaticCredentialsProvider(credentials))
          .withRegion(region)
          .build();
    }

    /**
     * Constructor for connecting to S3 with encryption.
     */
    public AwsS3Client(String awsAccessKey, String awsSecretKey, String regionName, String masterKey)
                       throws Exception {
        encryptionEnabled = true;
        this.masterKey = masterKey;

        Regions region;
        try {
            region = Regions.fromName(regionName);
        } catch (IllegalArgumentException e) {
            region = Regions.valueOf(regionName);
        }

        String masterKeyDir = System.getProperty("java.io.tmpdir");

        // Generate a symmetric 256-bit AES key.
        KeyGenerator symKeyGenerator = KeyGenerator.getInstance("AES");
        symKeyGenerator.init(256);
        SecretKey symKey = symKeyGenerator.generateKey();

        // To see how it works, save and load the key to and from the file system.
        saveSymmetricKey(masterKeyDir, masterKey, symKey);
        symKey = loadSymmetricAesKey(masterKeyDir, masterKey, "AES");

        EncryptionMaterials encryptionMaterials = new EncryptionMaterials(symKey);
        AWSCredentials credentials = new BasicAWSCredentials(awsAccessKey, awsSecretKey);

        s3client = AmazonS3EncryptionClientBuilder.standard()
            .withCredentials(new AWSStaticCredentialsProvider(credentials))
            .withRegion(region)
            .withEncryptionMaterials(new StaticEncryptionMaterialsProvider(encryptionMaterials))
            .build();
    }

    private static void saveSymmetricKey(String masterKeyDir, String masterKeyName, SecretKey secretKey)
                                         throws IOException {
        X509EncodedKeySpec x509EncodedKeySpec = new X509EncodedKeySpec(secretKey.getEncoded());
        FileOutputStream keyOutputStream = new FileOutputStream(masterKeyDir + File.separator + masterKeyName);
        keyOutputStream.write(x509EncodedKeySpec.getEncoded());
        keyOutputStream.close();
    }

    private static SecretKey loadSymmetricAesKey(String masterKeyDir, String masterKeyName, String algorithm)
            throws IOException, NoSuchAlgorithmException, InvalidKeySpecException, InvalidKeyException {
        // Read the key from the specified file.
        File keyFile = new File(masterKeyDir + File.separator + masterKeyName);
        FileInputStream keyInputStream = new FileInputStream(keyFile);
        byte[] encodedPrivateKey = new byte[(int) keyFile.length()];
        keyInputStream.read(encodedPrivateKey);
        keyInputStream.close();

        // Reconstruct and return the master key.
        return new SecretKeySpec(encodedPrivateKey, "AES");
    }

    /**
     * Default copy constructor.
     */
    public AwsS3Client(AmazonS3 s3client) {
        this.s3client = s3client;
    }

    /**
     * Populate the object.
     */
    public PutObjectResult putObject(String bucketName, String key, File file) {
        return s3client.putObject(bucketName, key, file);
    }

    /**
     * Get the object.
     */
    public S3Object getObject(String bucketName, String objectKey) {
        return s3client.getObject(bucketName, objectKey);
    }

    //deleting an object
    public void deleteObject(String bucketName, String objectKey) {
        s3client.deleteObject(bucketName, objectKey);
    }
}
