/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.KeyStore;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLException;

import org.apache.http.client.HttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.ssl.SSLContexts;
import org.springframework.http.HttpStatus;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;

import com.google.common.net.InetAddresses;
import com.vmware.blockchain.deployment.services.exception.ErrorCode;
import com.vmware.blockchain.deployment.services.exception.FileNotFoundPersephoneException;
import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.services.orchestration.ipam.IpamClient;
import com.vmware.blockchain.deployment.v1.Address;

import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import io.grpc.netty.shaded.io.grpc.netty.GrpcSslContexts;
import io.grpc.netty.shaded.io.grpc.netty.NettyChannelBuilder;
import lombok.extern.slf4j.Slf4j;

/**
 * common utilities for orchestrators.
 */
@Slf4j
public class OrchestratorUtils {

    /**
     * get ip address to allocate.
     * @param ipamClient ipamClient
     * @param networkSegmentName networkSegmentName
     * @param ip ip in string
     * @param name name
     * @return pair of name and ip address as string.
     */
    public static AbstractMap.SimpleEntry<String, String> getAddress(IpamClient ipamClient, String networkSegmentName,
                                                                     String ip, String name) {
        if (ip.isEmpty()) {
            Address add = ipamClient.allocatedPrivateIp(networkSegmentName);
            return new AbstractMap.SimpleEntry<>(add.getName(),
                    InetAddresses.fromInteger(add.getValue()).getHostAddress());
        }

        if (!InetAddresses.isInetAddress(ip)) {
            throw new IllegalArgumentException("Provided ip is not a valid Inet address. ip: " + ip);
        }

        return new AbstractMap.SimpleEntry<>(name, ip);
    }

    /**
     * get a HttpComponentsClientHttpRequestFactory given a keyStore to be incorporated in SSLContext
     * @param keyStore keyStore
     * @return HttpComponentsClientHttpRequestFactory with the keyStore incorporated in SSLContext.
     */
    public static HttpComponentsClientHttpRequestFactory getHttpRequestFactoryGivenKeyStore(KeyStore keyStore) {
        try {
            final SSLContext sslContext = SSLContexts.custom().loadTrustMaterial(keyStore, null).build();

            HttpClient httpClient = HttpClients.custom().setSSLContext(sslContext).build();

            HttpComponentsClientHttpRequestFactory factory = new HttpComponentsClientHttpRequestFactory(httpClient);
            return factory;
        } catch (Exception e) {
            throw new PersephoneException(e, ErrorCode.SSL_CONTEXT_CREATION_ERROR);
        }
    }

    /**
     * Adds self-signed certificates to SSL Context to be used for the gRPC call.
     * */
    public static ManagedChannel getSecureManagedChanel(String address, String pathToCerts) {
        try {
            List<File> files = getCertFiles(pathToCerts);

            List<X509Certificate> certificates = new ArrayList<>();
            CertificateFactory certFactory = CertificateFactory.getInstance("X.509");

            for (File file : files) {
                Certificate cert = null;
                try {
                    cert = certFactory.generateCertificate(new FileInputStream(file));
                } catch (CertificateException ce) {
                    if (ce.getMessage().contains("Could not parse")) {
                        //not stopping the certificate import, the folder might contain not only certificate
                        log.warn(file.getName() + " in certs folder is not a valid certificate. Skipping this file.");
                        continue;
                    } else {
                        throw ce;
                    }
                }
                certificates.add((X509Certificate) cert);
            }

            ManagedChannelBuilder channelBuilder = ManagedChannelBuilder.forTarget(address);
            return ((NettyChannelBuilder) channelBuilder).useTransportSecurity()
                    .sslContext(GrpcSslContexts.forClient().trustManager(certificates).build()).build();

        } catch (SSLException | CertificateException | FileNotFoundException e) {
            throw new PersephoneException(HttpStatus.INTERNAL_SERVER_ERROR, e, ErrorCode.GRPC_SSL_INIT_ERROR);
        }
    }

    private static List<File> getCertFiles(String filesPath) {
        List<File> files;
        try {
            Path path = Path.of(filesPath);
            if (!path.toFile().exists()) {
                throw new FileNotFoundPersephoneException(HttpStatus.INTERNAL_SERVER_ERROR,
                                                   ErrorCode.GRPC_SSL_INIT_ERROR, "Certificates folder does not exit.");
            }
            files = Files.find(path, 5, (p, basicFileAttributes) -> basicFileAttributes.isRegularFile())
                    .map(Path::toFile).collect(Collectors.toList());
            if (files.isEmpty()) {
                throw new FileNotFoundPersephoneException(HttpStatus.INTERNAL_SERVER_ERROR,
                                                            ErrorCode.GRPC_SSL_INIT_ERROR, "No certificates provided.");
            }

        } catch (IOException e) {
            throw new FileNotFoundPersephoneException(HttpStatus.INTERNAL_SERVER_ERROR, e,
                                                          ErrorCode.GRPC_SSL_INIT_ERROR, "Error reading certificates.");
        }
        return files;
    }

}
