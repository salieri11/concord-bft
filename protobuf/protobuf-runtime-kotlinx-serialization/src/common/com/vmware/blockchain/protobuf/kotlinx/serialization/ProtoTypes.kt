/*
 * Copyright 2018 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.vmware.blockchain.protobuf.kotlinx.serialization

import kotlinx.serialization.*
import kotlinx.serialization.protobuf.ProtoNumberType
import kotlinx.serialization.protobuf.ProtoType

internal typealias ProtoDesc = Pair<Int, ProtoNumberType>

internal fun extractParameters(desc: SerialDescriptor, index: Int): ProtoDesc {
    val idx = desc.findAnnotation<SerialId>(index)?.id ?: index + 1
    val format = desc.findAnnotation<ProtoType>(index)?.type
            ?: ProtoNumberType.DEFAULT
    return idx to format
}

inline fun <reified A: Annotation> SerialDescriptor.findAnnotation(elementIndex: Int): A? {
    val candidates = getElementAnnotations(elementIndex).filterIsInstance<A>()
    return when (candidates.size) {
        0 -> null
        1 -> candidates[0]
        else -> throw IllegalStateException("There are duplicate annotations of type ${A::class} in the descriptor $this")
    }
}
