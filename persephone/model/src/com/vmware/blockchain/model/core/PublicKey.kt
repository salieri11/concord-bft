/* **********************************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.model.core

/**
 * Represent the metadata and content of a public key.
 *
 * Note: This model may be revised. Specifically, enums and some form of a ByteString should be used
 * instead of String for every field.
 *
 * @property[algorithm]
 *   standard algorithm name for the key.
 * @property[base64Encoding]
 *   content of the key in base64 encoding.
 */
data class PublicKey(val algorithm: String, val base64Encoding: String)
