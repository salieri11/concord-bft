/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;

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

    private UserRepository userRepository;

    private OrganizationRepository organizationRepository;

    private ConsortiumRepository consortiumRepository;

    private PasswordEncoder passwordEncoder;

    private BlockchainRepository blockchainRepository;

    private String blockchainNodes;


    @Autowired
    public DefaultProfiles(
            UserRepository userRepository,
            OrganizationRepository organizationRepository,
            ConsortiumRepository consortiumRepository,
            PasswordEncoder passwordEncoder,
            BlockchainRepository blockchainRepository,
            @Value("${ConcordAuthorities}") String blockchainNodes) {
        this.userRepository = userRepository;
        this.organizationRepository = organizationRepository;
        this.consortiumRepository = consortiumRepository;
        this.passwordEncoder = passwordEncoder;
        this.blockchainRepository = blockchainRepository;
        this.blockchainNodes = blockchainNodes;
    }

    // TODO: These next few methords are just testing convenience methods and should be removed
    // when actual POST API for organization and consortium creation is
    // available

    /**
     * Create the default profiles if they don't exist.  Do this when the application goes ready.
     */
    @EventListener(classes = ApplicationReadyEvent.class)
    public void initialize() {
        // the order of these creates matters.
        // consortium must exist before blockchain.
        // both consortium and organization must exist before user.
        organization = createOrgIfNotExist();
        consortium = createConsortiumIfNotExist();
        blockchain = createBlockchainIfNotExist();
        user = createUserIfNotExist();
        logger.info("Profiles -- Org {}, Cons: {}, BC: {}, User: {}", organization.getOrganizationName(),
                consortium.getConsortiumName(), blockchain.getIpAsList(), user.getEmail());
    }

    private User createUserIfNotExist() {
        logger.info("Application ready");
        String email = "admin@blockchain.local";
        String password = "Admin!23";
        List<User> oUser = userRepository.findAll();
        if (oUser.isEmpty()) {
            logger.info("Creating Initial User");
            User u = new User();
            u.setName("ADMIN");
            u.setEmail(email);
            u.setPassword(passwordEncoder.encode(password));
            u.setRole(Roles.get("SYSTEM_ADMIN"));
            u.setOrganization(organization);
            u.setConsortium(consortium);
            // Note: The order of next 5 statements is very important, The user
            // object must be saved before it can be added and saved into
            // consortium & organization objects.
            u = userRepository.save(u);
            organization.addUser(u);
            consortium.addUser(u);
            logger.info("Admin user created. Username: {} Password: {}", email, password);
            return u;
        } else {
            return oUser.get(0);
        }
    }

    private Organization createOrgIfNotExist() {
        List<Organization> oList = organizationRepository.findAll();
        if (oList.isEmpty()) {
            Organization o = new Organization();
            o.setOrganizationName("ADMIN");
            o = organizationRepository.save(o);
            return o;
        } else {
            return oList.get(0);
        }
    }

    private Consortium createConsortiumIfNotExist() {
        List<Consortium> cList = consortiumRepository.findAll();
        if (cList.isEmpty()) {
            Consortium c = new Consortium();
            c.setConsortiumName("ADMIN");
            c.setConsortiumType("ADMIN");
            c = consortiumRepository.save(c);
            return c;
        } else {
            return cList.get(0);
        }
    }

    private Blockchain createBlockchainIfNotExist() {
        List<Blockchain> bList = blockchainRepository.findAll();
        if (bList.isEmpty()) {
            Blockchain b = new Blockchain();
            b.setConsortium(consortium);
            b.setIpList(blockchainNodes);
            b = blockchainRepository.save(b);
            return b;
        } else {
            return bList.get(0);
        }
    }

}
