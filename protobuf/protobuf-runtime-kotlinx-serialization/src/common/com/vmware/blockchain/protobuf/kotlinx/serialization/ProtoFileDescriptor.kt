/* **********************************************************************
 * Copyright 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.protobuf.kotlinx.serialization

/**
 * Denotes a Protocol Buffer file descriptor definition (encoded) along with its dependencies in
 * the form of other file descriptor instances.
 */
interface ProtoFileDescriptor {

    /** Encoded of file descriptor content sans optional source_code_info section. */
    val encodedData: String

    /** File descriptors that this instance's definition is dependent upon. */
    val dependencies: List<ProtoFileDescriptor>

    /**
     * Resolve all [ProtoFileDescriptor] that this instance is transitively dependent on.
     */
    fun resolveDependencies(): Set<ProtoFileDescriptor> {
        val resolved = mutableSetOf(this)
        dependencies.flatMapTo(resolved) { it.resolveDependencies() }

        return resolved
    }
}
