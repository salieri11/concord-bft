/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 */

package services.profiles;

import org.springframework.data.jpa.repository.JpaRepository;

public interface OrganizationRepository extends
        JpaRepository<Organization, Long> {
}
