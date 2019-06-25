/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.configuration.generatecerts;

import java.util.List;

import com.vmware.blockchain.deployment.model.ConfigurationServiceType;
import com.vmware.blockchain.deployment.model.Identity;
import com.vmware.blockchain.deployment.model.IdentityFactors;

/**
 * This interface encapsulates ssl certificate(s) generator.
 */
public interface CertificatesGenerator {

    /** concord security certificates generation path. */
    String CONCORD_TLS_SECURITY_IDENTITY_PATH = "/concord/config-local/cert";

    /** ethrpc security certificates generation path. */
    String CONCORD_ETHRPC_SECURITY_IDENTITY_PATH = "/ethrpc/cert";

    /**
     * Method to generate ssl certificates and keypar.
     */
    List<Identity> generateSelfSignedCertificates(int numCerts, ConfigurationServiceType.DockerType type);

    /**
     * Method to get the identity factor for the certificates generated.
     */
    IdentityFactors getIdentityFactor();
}
