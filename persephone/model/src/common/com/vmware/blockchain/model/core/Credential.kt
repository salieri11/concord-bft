package com.vmware.blockchain.model.core

import kotlinx.serialization.Serializable

/**
 * Union of all credential types.
 *
 * Note:
 * Ideally this should be declared as a sealed type that has one abstract property "type" and all
 * subtypes will specify exactly 1 type and define additional properties. Doing so however
 * complicates the deserialization process (i.e. naive deserialization of a polymorphic
 * discriminated union is lossy).
 *
 * Note:
 * This model currently does not enable secure handling sensitive information by itself.

 * @property[type]
 *   type of the credential.
 * @property[passwordCredential]
 *   content of password-based credential.
 * @property[tokenCredential]
 *   content of token-based credential.
 */
@Serializable
data class Credential(
    val type: Type = Type.NONE,
    val passwordCredential: PasswordCredential? = null,
    val tokenCredential: BearerTokenCredential? = null
) {
    /**
     * Enumeration of possible [Credential] types.
     */
    enum class Type { NONE, PASSWORD, BEARER }
}

/**
 * Denote a basic authentication credential.
 *
 * @property[username]
 *   login username to present to the endpoint.
 * @property[password]
 *   login password to present to the endpoint.
 */
@Serializable
data class PasswordCredential(val username: String, val password: String)

/**
 * Denote a token authentication credential.
 *
 * @property[token]
 *   token value to present to the endpoint.
 */
@Serializable
data class BearerTokenCredential(val token: String)
