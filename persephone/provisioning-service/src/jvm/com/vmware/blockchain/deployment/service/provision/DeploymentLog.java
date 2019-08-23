/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.service.provision;

import java.util.Collections;

import org.reactivestreams.Publisher;

import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore;
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Versioned;
import com.vmware.blockchain.deployment.persistence.kv.MonotonicInt;
import com.vmware.blockchain.deployment.reactive.MappingPublisher;
import com.vmware.blockchain.deployment.v1.ConcordClusterIdentifier;
import com.vmware.blockchain.deployment.v1.DeploymentSession;
import com.vmware.blockchain.deployment.v1.DeploymentSessionEvent;
import com.vmware.blockchain.deployment.v1.DeploymentSessionIdentifier;
import com.vmware.blockchain.deployment.v1.DeploymentSpecification;
import com.vmware.blockchain.deployment.v1.PlacementAssignment;

/**
 * Provisioning service log/event related class.
 */
public class DeploymentLog {

    /** Initial version value of all stored deployment sessions. */
    private static MonotonicInt initialVersion = new MonotonicInt().next();

    /** Internal storage for deployment session log entries. */
    private final KeyValueStore<DeploymentSessionIdentifier, DeploymentSession, MonotonicInt> store;

    /**
     * Constructor.
     */
    public DeploymentLog(
            KeyValueStore<DeploymentSessionIdentifier, DeploymentSession, MonotonicInt> store
    ) {
        this.store = store;
    }

    Publisher<DeploymentSession> createEntry(
            DeploymentSessionIdentifier identifier,
            DeploymentSpecification model
    ) {
        var session = new DeploymentSession(
                identifier,
                model,
                ConcordClusterIdentifier.Companion.getDefaultValue(),
                new PlacementAssignment(),
                DeploymentSession.Status.ACTIVE,
                Collections.emptyList()
        );

        // Record the session and piggyback on its completion.
        var stored = store.set(identifier, initialVersion, session);
        return new MappingPublisher<>(stored, versioned -> {
            if (versioned == Versioned.None.INSTANCE) {
                return session;
            } else {
                // There was a prior value already.
                throw new IllegalStateException("Entry with session identifier already exist");
            }
        });
    }

    void updateEntry(DeploymentSessionIdentifier identifier, DeploymentSessionEvent event) {
        // Fetch the existing session.
    }

    void completeEntry(DeploymentSessionIdentifier identifier) {
    }

    void completeEntry(DeploymentSessionIdentifier identifier, Throwable error) {
    }
}
