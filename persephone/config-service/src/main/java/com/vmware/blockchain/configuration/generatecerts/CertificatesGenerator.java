/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generatecerts;

import java.util.List;

import com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType;
import com.vmware.blockchain.deployment.v1.Identity;
import com.vmware.blockchain.deployment.v1.IdentityFactors;

/**
 * This interface encapsulates ssl certificate(s) generator.
 */
public interface CertificatesGenerator {

    /** common prefix. */
    String FILE_PREFIX = "file:";

    /** config prefix. */
    String IDENTITY_PATH_PREFIX = "/config";

    /** concord security certificates generation path. */
    String CONCORD_TLS_SECURITY_IDENTITY_PATH = "/concord/config-local/cert";

    /** bft client security certificates generation path. */
    String BFT_CLIENT_TLS_SECURITY_IDENTITY_PATH = "/daml-ledger-api/config-local/cert";

    /** ethrpc security certificates generation path. */
    String CONCORD_ETHRPC_SECURITY_IDENTITY_PATH = "/ethrpc/cert";

    /** trc tls certtificates generation path. */
    String TRC_TLS_IDENTITY_PATH = "/daml-ledger-api/config-local/trc_tls_certs";

    /** trs tls certtificates generation path. */
    String TRS_TLS_IDENTITY_PATH = "/concord/config-local/trs_tls_certs";

    /**
     * Method to generate ssl certificates and keypair.
     */
    List<Identity> generateSelfSignedCertificates(int numCerts, ServiceType type);

    /**
     * Method to generate ssl certificates and keypair when CN and OU provided.
     */
    List<Identity> generateSelfSignedCertificates(int numCerts, ServiceType type, String cn, String ou);

    /**
     * Method to get the identity factor for the certificates generated.
     */
    IdentityFactors getIdentityFactor();
}
