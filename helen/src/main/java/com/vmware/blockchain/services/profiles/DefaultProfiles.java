/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Collections;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.security.ServiceContext;

import lombok.Getter;

/**
 * Component that contains default profiles for development.
 * Create the initial user, organization, consortium and blockchain if they  don't
 * exist.
 */
@Component
public class DefaultProfiles {
    private Logger logger = LogManager.getLogger(DefaultProfiles.class);
    @Getter
    private User user;

    @Getter
    private Organization organization;

    @Getter
    private Consortium consortium;

    @Getter
    private Blockchain blockchain;

    private AgreementService agreementService;

    private UserService userService;

    private OrganizationService organizationService;

    private ConsortiumService consortiumService;

    private PasswordEncoder passwordEncoder;

    private BlockchainService blockchainService;

    private ServiceContext serviceContext;

    private String blockchainIpList;

    private String blockchainRpcUrls;

    private String blockchainRpcCerts;


    @Autowired
    public DefaultProfiles(
            UserService userService,
            OrganizationService organizationService,
            ConsortiumService consortiumService,
            PasswordEncoder passwordEncoder,
            BlockchainService blockchainService,
            AgreementService agreementService,
            ServiceContext serviceContext,
            @Value("${ConcordAuthorities}") String blockchainIpList,
            @Value("${ConcordRpcUrls}") String blockchainRpcUrls,
            @Value("${ConcordRpcCerts}") String blockchainRpcCerts) {
        this.userService = userService;
        this.organizationService = organizationService;
        this.consortiumService = consortiumService;
        this.passwordEncoder = passwordEncoder;
        this.blockchainService = blockchainService;
        this.agreementService = agreementService;
        this.serviceContext = serviceContext;
        this.blockchainIpList = blockchainIpList;
        this.blockchainRpcUrls = blockchainRpcUrls;
        this.blockchainRpcCerts = blockchainRpcCerts;
    }

    // TODO: These next few methords are just testing convenience methods and should be removed
    // when actual POST API for organization and consortium creation is
    // available

    /**
     * Create the default profiles if they don't exist.
     */
    public void initialize() {
        // the order of these creates matters.
        // consortium must exist before blockchain.
        // both consortium and organization must exist before user.
        // We need to set the service security context first, in order to create the entities.
        serviceContext.setSystemContext();
        createAgreementIfNotExist();
        organization = createOrgIfNotExist();
        consortium = createConsortiumIfNotExist();
        blockchain = createBlockchainIfNotExist();
        user = createUserIfNotExist();
        serviceContext.clearServiceContext();
        logger.info("Profiles -- Org {}, Cons: {}, BC: {}, User: {}", organization.getOrganizationName(),
                consortium.getConsortiumName(), blockchain.getUrlsAsMap(), user.getEmail());
    }

    private User createUserIfNotExist() {
        logger.info("Application ready");
        String email = "admin@blockchain.local";
        String password = "Admin!23";
        List<User> oUser = userService.list();
        if (oUser.isEmpty()) {
            logger.info("Creating Initial User");
            User u = new User();
            u.setName("ADMIN");
            u.setEmail(email);
            u.setPassword(passwordEncoder.encode(password));
            u.setRoles(Collections.singletonList(Roles.get("SYSTEM_ADMIN")));
            u.setOrganization(organization.getId());
            // Note: The order of next 5 statements is very important, The user
            // object must be saved before it can be added and saved into
            // consortium & organization objects.
            u = userService.put(u);
            logger.info("Admin user created. Username: {} Password: {}", email, password);
            return u;
        } else {
            return oUser.get(0);
        }
    }

    private Organization createOrgIfNotExist() {
        List<Organization> oList = organizationService.list();
        if (oList.isEmpty()) {
            Organization o = new Organization();
            o.setOrganizationName("ADMIN");
            o = organizationService.put(o);
            return o;
        } else {
            return oList.get(0);
        }
    }

    private Consortium createConsortiumIfNotExist() {
        List<Consortium> cList = consortiumService.list();
        if (cList.isEmpty()) {
            Consortium c = new Consortium();
            c.setConsortiumName("ADMIN");
            c.setConsortiumType("ADMIN");
            c.setOrganization(organization.getId());
            c = consortiumService.put(c);
            return c;
        } else {
            return cList.get(0);
        }
    }

    private Blockchain createBlockchainIfNotExist() {
        List<Blockchain> bList = blockchainService.list();
        if (bList.isEmpty()) {
            return blockchainService.create(consortium, blockchainIpList, blockchainRpcUrls, blockchainRpcCerts);
        } else {
            return bList.get(0);
        }
    }

    private Agreement createAgreementIfNotExist() {
        try {
            return agreementService.getAgreementWithId(null);
        } catch (NotFoundException e) {
            return agreementService.createAgreement();
        }
    }

}
