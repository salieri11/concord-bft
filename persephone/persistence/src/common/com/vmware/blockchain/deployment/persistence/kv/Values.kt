/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.persistence.kv

import kotlinx.serialization.KSerializer

/**
 * Convert this [TypedValue] instance to an [UntypedValue] instance.
 */
fun <T> TypedValue<T>.toUntypedValue(): UntypedValue = UntypedValue(asByteArray())

/**
 * Convert this [UntypedValue] instance to a [TypedValue] instance.
 *
 * @param[serializer]
 *   serializer to use for deserialization.
 */
fun <T> UntypedValue.toTypedValue(serializer: KSerializer<T>) = TypedValue(asByteArray(), serializer)
