/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import org.springframework.data.jpa.repository.JpaRepository;

public interface ConsortiumRepository extends JpaRepository<Consortium, Long> {
}
