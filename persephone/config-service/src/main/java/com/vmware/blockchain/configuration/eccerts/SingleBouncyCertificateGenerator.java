/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.eccerts;

import java.io.IOException;
import java.io.StringWriter;
import java.math.BigInteger;
import java.security.InvalidAlgorithmParameterException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.SecureRandom;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.Calendar;
import java.util.Date;

import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.asn1.x500.X500NameBuilder;
import org.bouncycastle.asn1.x500.style.BCStyle;
import org.bouncycastle.asn1.x509.AuthorityKeyIdentifier;
import org.bouncycastle.asn1.x509.BasicConstraints;
import org.bouncycastle.asn1.x509.Extension;
import org.bouncycastle.asn1.x509.SubjectKeyIdentifier;
import org.bouncycastle.cert.X509CertificateHolder;
import org.bouncycastle.cert.X509v3CertificateBuilder;
import org.bouncycastle.cert.jcajce.JcaX509CertificateConverter;
import org.bouncycastle.cert.jcajce.JcaX509v3CertificateBuilder;
import org.bouncycastle.jce.ECNamedCurveTable;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.jce.spec.ECParameterSpec;
import org.bouncycastle.openssl.jcajce.JcaPEMWriter;
import org.bouncycastle.operator.ContentSigner;
import org.bouncycastle.operator.OperatorCreationException;
import org.bouncycastle.operator.jcajce.JcaContentSignerBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.v1.Identity;
import com.vmware.blockchain.deployment.v1.IdentityComponent;


/**
 * Class to generate keypair and certificate using {@link BouncyCastleProvider}.
 */
public class SingleBouncyCertificateGenerator {

    private static final SecureRandom random = new SecureRandom();
    private static Logger log = LoggerFactory.getLogger(SingleBouncyCertificateGenerator.class);

    /**
     * Generates the X509Certificate and private key pair.
     * @param path : string path to where it should be generated in
     * @param cn : subject cn value
     * @param ou : subject ou value
     * @return : {@link Identity}
     */
    static Identity generateIdentity(String path, String cn, String ou) {
        KeyPair keyPair = generateEcKeyPair();
        X509Certificate certificate = generateSelfSignedCertificate(keyPair, cn, ou);
        return getIdentity(keyPair, certificate, path);
    }

    /**
     * Generates {@link X509Certificate}.
     * @param keypair {@link KeyPair}
     * @param cn : subject cn value
     * @param ou : subject ou value
     * @return {@link X509Certificate}
     */
    private static X509Certificate generateSelfSignedCertificate(KeyPair keypair, String cn, String ou) {

        byte[] id = new byte[20];
        BigInteger serial = new BigInteger(160, random);
        X500Name subject = new X500NameBuilder()
                .addRDN(BCStyle.C, "NA")
                .addRDN(BCStyle.ST, "NA")
                .addRDN(BCStyle.L, "NA")
                .addRDN(BCStyle.O, "NA")
                .addRDN(BCStyle.OU, ou)
                .addRDN(BCStyle.CN, cn)
                .build();
        Date fromDate = new Date();
        Calendar c = Calendar.getInstance();
        c.setTime(fromDate);
        c.add(Calendar.YEAR, 3);
        Date toDate = c.getTime();

        X509v3CertificateBuilder certificateBuilder = new JcaX509v3CertificateBuilder(
                subject, serial, fromDate, toDate, subject, keypair.getPublic());

        try {
            SubjectKeyIdentifier subjKeyId = new SubjectKeyIdentifier(keypair.getPublic().getEncoded());
            certificateBuilder.addExtension(Extension.subjectKeyIdentifier, false, subjKeyId);

            AuthorityKeyIdentifier authKeyId = new AuthorityKeyIdentifier(keypair.getPublic().getEncoded());
            certificateBuilder.addExtension(Extension.authorityKeyIdentifier, false, authKeyId);

            BasicConstraints constraints = new BasicConstraints(true);
            certificateBuilder.addExtension(Extension.basicConstraints,
                    true, constraints.getEncoded());

            ContentSigner signer = new JcaContentSignerBuilder("SHA256withECDSA").build(keypair.getPrivate());
            X509CertificateHolder holder = certificateBuilder.build(signer);

            JcaX509CertificateConverter converter = new JcaX509CertificateConverter();
            converter.setProvider(new BouncyCastleProvider());
            X509Certificate certificate = converter.getCertificate(holder);

            /* Serialize
            byte[] serialized = x509.getEncoded();
            */

            return certificate;
        } catch (IOException | OperatorCreationException | CertificateException  e) {
            log.error("Failed to generate certificate ");
            e.printStackTrace();
            throw new RuntimeException(e);
        }
    }

    /**
     * Generates {@link KeyPair}.
     * @return {@link KeyPair}
     */
    private static KeyPair generateEcKeyPair() {
        try {
            ECParameterSpec ecSpec = ECNamedCurveTable.getParameterSpec("secp384r1");
            KeyPairGenerator kpGen = KeyPairGenerator.getInstance("ECDSA", "BC");
            kpGen.initialize(ecSpec, random);
            return kpGen.generateKeyPair();
        } catch (NoSuchProviderException | NoSuchAlgorithmException | InvalidAlgorithmParameterException e) {
            log.error("Failed to generate key pair");
            e.printStackTrace();
            throw new RuntimeException(e);
        }
    }

    /**
     * Writes the private key and certifitcates in given folder.
     * @param keyPair : {@link KeyPair}
     * @param  certificate : {@link X509Certificate}
     * @param path root path to dump certs and keys
     * @return {@link Identity}
     */
    private static Identity getIdentity(KeyPair keyPair, X509Certificate certificate, String path) {
        // get string private key
        String keyPath = String.join("/", path, "pk.pem");
        String key = returnPemString(keyPair.getPrivate());

        IdentityComponent keyIdentity = new IdentityComponent(
                IdentityComponent.Type.KEY, keyPath, key);

        // get string cert
        String certPath = String.join("/", path,
                path.substring(path.lastIndexOf("/") + 1) + ".cert");
        String cert = returnPemString(certificate);

        IdentityComponent certIdentity = new IdentityComponent(
                IdentityComponent.Type.CERTIFICATE, certPath, cert);

        return new Identity(keyIdentity, certIdentity);
    }

    /**
     * returns PEM string format of keys/certs
     * @param object : {@link java.security.Key} or {@link X509Certificate} object
     * @return PEM String
     */
    private static String returnPemString(Object object) {
        try {
            StringWriter writer = new StringWriter();
            JcaPEMWriter pemWriter = new JcaPEMWriter(writer);
            pemWriter.writeObject(object);
            pemWriter.close();
            String ecCert = writer.toString();
            writer.close();
            return ecCert;
        } catch (IOException e) {
            log.error("Creating string certificate/key failed");
            e.printStackTrace();
            throw new RuntimeException(e);
        }

    }
}
