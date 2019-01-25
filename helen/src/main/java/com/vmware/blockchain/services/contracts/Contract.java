/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.contracts;

import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import org.hibernate.annotations.Generated;
import org.hibernate.annotations.GenerationTime;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Contract Data type for JPA.
 */
@Table(name = "CONTRACTS")
@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Contract {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private UUID id;

    @Column(name = "contract_id", nullable = false)
    private String name;

    @Column(name = "version_name", nullable = false)
    private String versionName;

    private String address;

    private String sourcecode;

    private String bytecode;

    private String metadata;

    private String owner;

    @Generated(GenerationTime.ALWAYS)
    @Column(name = "sequence_number", insertable = false, updatable = false)
    private long seq;

    @Column(name = "blockchain_id", nullable = false)
    private UUID blockchainId;

}
