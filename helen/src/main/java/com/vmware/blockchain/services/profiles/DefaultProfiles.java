/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.common.ConcordConnectionException;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.security.ServiceContext;
import com.vmware.blockchain.services.blockchains.Blockchain;
import com.vmware.blockchain.services.blockchains.Blockchain.NodeEntry;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.blockchains.replicas.Replica;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;
import com.vmware.blockchain.services.concord.ConcordService;
import com.vmware.concord.Concord.Peer;

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

    private ReplicaService replicaService;

    private ConcordService concordService;

    private ServiceContext serviceContext;

    private String blockchainIpList;

    private String blockchainRpcUrls;

    private String blockchainRpcCerts;

    private boolean createDefaultBlockchain;

    private UUID defaultOrgId;


    @Autowired
    public DefaultProfiles(
            UserService userService,
            OrganizationService organizationService,
            ConsortiumService consortiumService,
            PasswordEncoder passwordEncoder,
            BlockchainService blockchainService,
            AgreementService agreementService,
            ServiceContext serviceContext,
            ReplicaService replicaService,
            ConcordService concordService,
            @Value("${ConcordAuthorities}") String blockchainIpList,
            @Value("${ConcordRpcUrls}") String blockchainRpcUrls,
            @Value("${ConcordRpcCerts}") String blockchainRpcCerts,
            @Value("${vmbc.default.blockchain:false}") boolean createDefaultBlockchain,
            @Value("${default.profile.org.id:#{null}}") UUID defaultOrgId) {
        this.userService = userService;
        this.organizationService = organizationService;
        this.consortiumService = consortiumService;
        this.passwordEncoder = passwordEncoder;
        this.blockchainService = blockchainService;
        this.agreementService = agreementService;
        this.serviceContext = serviceContext;
        this.replicaService = replicaService;
        this.concordService = concordService;
        this.blockchainIpList = blockchainIpList;
        this.blockchainRpcUrls = blockchainRpcUrls;
        this.blockchainRpcCerts = blockchainRpcCerts;
        this.createDefaultBlockchain = createDefaultBlockchain;
        this.defaultOrgId = defaultOrgId;
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
        if (createDefaultBlockchain) {
            blockchain = createBlockchainIfNotExist();
        } else {
            // We need an empty blockchain backwards compatibility.  Some old calls are failing without this.
            blockchain = new Blockchain();
        }
        user = createUserIfNotExist();
        serviceContext.clearServiceContext();
        List<String> nodeInfo = Collections.emptyList();
        if (blockchain.getNodeList() != null) {
            // For blockchain, don't log the node cert
            nodeInfo = blockchain.getNodeList().stream()
                    .map(n -> String.format("%s %s %s", n.getHostName(), n.getIp(), n.getUrl()))
                    .collect(Collectors.toList());
        }
        logger.info("Profiles -- Org {}, Cons: {}, BC: {}, User: {}", organization.getOrganizationName(),
                consortium.getConsortiumName(), nodeInfo, user.getEmail());
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
            u.setRoles(ImmutableList.of(Roles.ORG_USER, Roles.SYSTEM_ADMIN));
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
            if (defaultOrgId != null) {
                o.setId(defaultOrgId);
            }
            o.setOrganizationProperties(ImmutableMap.of(Constants.ORG_MAX_CHAINS, "0"));
            o = organizationService.put(o);
            logger.info("Org created, orgId: {}", o.getId());
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
            Blockchain b =
                    blockchainService.create(consortium, blockchainIpList, blockchainRpcUrls, blockchainRpcCerts);
            createReplicas(b);
            return b;
        } else {
            return bList.get(0);
        }
    }

    private void createReplicas(Blockchain b) {
        try {
            List<Peer> peers = concordService.getMembers(b.getId());
            List<NodeEntry> nodes = b.getNodeList();
            Replica.ReplicaType replicaType = b.getType() == Blockchain.BlockchainType.DAML
                    ? Replica.ReplicaType.DAML_COMMITTER
                    : Replica.ReplicaType.NONE;
            for (int i = 0; i < nodes.size(); i++) {
                NodeEntry n = nodes.get(i);
                Peer p = peers.get(i);
                Replica r = new Replica(n.getIp(),
                                        p.getAddress().split(":")[0],
                                        p.getHostname(), n.getUrl(), n.getCert(), n.getZoneId(), replicaType,
                                        b.getId());
                r.setId(n.getNodeId());
                replicaService.put(r);
            }
        }
        catch (ConcordConnectionException e) {
            logger.error("Concord internal error: Unable to get concord connection");
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
