/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;

/**
 * JPA repository for Organizations.
 */
public interface OrganizationRepository extends JpaRepository<Organization, UUID> {
}
