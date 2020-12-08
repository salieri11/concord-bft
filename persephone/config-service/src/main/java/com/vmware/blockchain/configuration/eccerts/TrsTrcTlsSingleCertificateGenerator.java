/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.eccerts;

import java.util.List;

import com.vmware.blockchain.configuration.generatecerts.CertificatesGenerator;
import com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType;
import com.vmware.blockchain.deployment.v1.Identity;
import com.vmware.blockchain.deployment.v1.IdentityFactors;

/**
 * This class is a bouncycastle implementation of getting ssl certs and keypair.
 */
public class TrsTrcTlsSingleCertificateGenerator implements CertificatesGenerator {

    @Override
    public List<Identity> generateSelfSignedCertificates(int numCerts, ServiceType type) {
        throw new UnsupportedOperationException("CN and OU needs to be provided for TRC and TRS certificates.");
    }

    @Override
    public List<Identity> generateSelfSignedCertificates(int numCerts, ServiceType type, String cn, String ou) {
        if (numCerts == 0) {
            throw new IllegalArgumentException("Number of certificates can not be 0");
        }
        if (numCerts > 1) {
            throw new UnsupportedOperationException("Only single certificate generation is supported.");
        }
        String path;
        String certName;
        if (type.equals(ServiceType.CONCORD)) {
            path = FILE_PREFIX + TRS_TLS_IDENTITY_PATH;
            certName = "server.cert";
        } else if (type.equals(ServiceType.DAML_LEDGER_API)) {
            path = FILE_PREFIX + TRC_TLS_IDENTITY_PATH;
            certName = "client.cert";
        } else {
            throw new IllegalArgumentException("Certs only for TRS and TRC are supported.");
        }

        Identity identity = SingleBouncyCertificateGenerator.generateIdentity(path, certName, cn, ou, cn);
        return List.of(identity);
    }

    @Override
    public IdentityFactors getIdentityFactor() {
        return IdentityFactors.newBuilder()
                .setAlgorithm("ECDSA")
                .setCurve("secp384r1")
                .setSigningAlgorithm("SHA384WITHECDSA")
                .build();
    }
}
