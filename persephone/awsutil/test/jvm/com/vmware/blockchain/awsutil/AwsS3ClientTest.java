/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.awsutil;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.PutObjectResult;


/**
 * Test AwsS3Client for getting/putting object.
 */
public class AwsS3ClientTest {

    private static final String BUCKET_NAME = "config";
    private static final String KEY_NAME = "test.txt";

    private AmazonS3 s3;
    private AwsS3Client service;
    private AwsS3Client realService;

    /**
     * Setup is invoked prior to each testcase.
     */
    @BeforeEach
    public void setUp() {
        s3 = mock(AmazonS3.class);
        service = new AwsS3Client(s3);
        try (InputStream input = AwsS3ClientTest.class.getClassLoader().getResourceAsStream("application.properties")) {
            Properties prop = new Properties();
            if (input == null) {
                System.out.println("Sorry, unable to find application.properties");
                return;
            }

            //load a properties file from class path, inside static method
            prop.load(input);

            //get the property value and print it out
            String accessKey = prop.getProperty("aws.accessKey");
            String secretKey = prop.getProperty("aws.secretKey");
            String region = prop.getProperty("aws.region");
            realService = new AwsS3Client(accessKey, secretKey, region);
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    @Test
    public void whenVerifyingPutObject_thenCorrect() {
        File file = mock(File.class);
        PutObjectResult result = mock(PutObjectResult.class);
        when(s3.putObject(anyString(), anyString(), (File) any())).thenReturn(result);

        assertThat(service.putObject(BUCKET_NAME, KEY_NAME, file)).isEqualTo(result);
        verify(s3).putObject(BUCKET_NAME, KEY_NAME, file);
    }

    @Test
    public void whenVerifyingGetObject_thenCorrect() {
        service.getObject(BUCKET_NAME, KEY_NAME);
        verify(s3).getObject(BUCKET_NAME, KEY_NAME);
    }

    @Test
    public void whenVerifyingDeleteObject_thenCorrect() {
        service.deleteObject(BUCKET_NAME, KEY_NAME);
        verify(s3).deleteObject(BUCKET_NAME, KEY_NAME);
    }
}
