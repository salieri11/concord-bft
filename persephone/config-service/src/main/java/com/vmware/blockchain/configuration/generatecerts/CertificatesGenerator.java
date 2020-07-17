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

    /** concord security certificates generation path. */
    String CONCORD_TLS_SECURITY_IDENTITY_PATH = "file:/concord/config-local/cert";

    /** bft client security certificates generation path. */
    String BFT_CLIENT_TLS_SECURITY_IDENTITY_PATH = "file:/daml-ledger-api/config-local/cert";

    /** ethrpc security certificates generation path. */
    String CONCORD_ETHRPC_SECURITY_IDENTITY_PATH = "file:/ethrpc/cert";

    /**
     * Method to generate ssl certificates and keypar.
     */
    List<Identity> generateSelfSignedCertificates(int numCerts, ServiceType type);

    /**
     * Method to get the identity factor for the certificates generated.
     */
    IdentityFactors getIdentityFactor();
}
