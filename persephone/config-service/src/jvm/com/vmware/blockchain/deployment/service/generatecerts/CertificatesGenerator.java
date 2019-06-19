/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.generatecerts;

import java.util.List;

import com.vmware.blockchain.deployment.model.ConfigurationServiceType;
import com.vmware.blockchain.deployment.model.Identity;

/**
 * This interface encapsulates ssl certificate(s) generator.
 */
public interface CertificatesGenerator {

    /**
     * Method to generate ssl certificates and keypar.
     */
    List<Identity> generateSelfSignedCertificates(int numCerts, ConfigurationServiceType.Type type);
}
