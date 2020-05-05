// Copyright 2020 VMware, all rights reserved.

#include "openssl_crypto_utils.hpp"

#include <openssl/ec.h>
#include <openssl/evp.h>

using concord::utils::openssl_crypto::AsymmetricPrivateKey;
using concord::utils::openssl_crypto::AsymmetricPublicKey;
using concord::utils::openssl_crypto::UnexpectedOpenSSLCryptoFailureException;
using std::invalid_argument;
using std::pair;
using std::string;
using std::unique_ptr;

// Deleter classes for using smart pointers that correctly manage objects from
// the OpenSSL Crypto library, given OpenSSL Crypto objects use free functions
// provided by the library rather than normal destructors.
class BIGNUMDeleter {
 public:
  void operator()(BIGNUM* num) const { BN_clear_free(num); }
};
class BNCTXDeleter {
 public:
  void operator()(BN_CTX* obj) const { BN_CTX_free(obj); }
};
class ECKEYDeleter {
 public:
  void operator()(EC_KEY* obj) const { EC_KEY_free(obj); }
};
class ECPOINTDeleter {
 public:
  void operator()(EC_POINT* obj) const { EC_POINT_clear_free(obj); }
};
class EVPMDCTXDeleter {
 public:
  void operator()(EVP_MD_CTX* obj) const { EVP_MD_CTX_free(obj); }
};
class EVPPKEYDeleter {
 public:
  void operator()(EVP_PKEY* obj) const { EVP_PKEY_free(obj); }
};
class OPENSSLStringDeleter {
 public:
  void operator()(char* string) const { OPENSSL_free(string); }
};

// Wrapper class for OpenSSL Crypto's EVP_PKEY objects implementing
// concord::utils::openssl_crypto::AsymmetricPrivateKey.
class EVPPKEYPrivateKey : public AsymmetricPrivateKey {
 private:
  unique_ptr<EVP_PKEY, EVPPKEYDeleter> pkey;
  string scheme_name;

 public:
  // Copying and moving EVPPKEYPrivateKey objects is currently unimplemented;
  // note the default implementations for copy and move operations would not be
  // appropriate for this class since the EVP_PKEY object it contains must be
  // memory-managed through the OpenSSL library rather than by default
  // mechanisms.
  EVPPKEYPrivateKey(const EVPPKEYPrivateKey& other) = delete;
  EVPPKEYPrivateKey(const EVPPKEYPrivateKey&& other) = delete;
  EVPPKEYPrivateKey& operator=(const EVPPKEYPrivateKey& other) = delete;
  EVPPKEYPrivateKey& operator=(const EVPPKEYPrivateKey&& other) = delete;

  // Note the constructed EVPPKEYPrivateKey takes ownership of the pointer it is
  // constructed with. A precondition of this constructor is that the provided
  // EVP_PKEY object must have both its private and public keys initialized;
  // future behavior of this EVPPKEYPrivateKey object is undefined if this
  // precondition is not met.
  EVPPKEYPrivateKey(unique_ptr<EVP_PKEY, EVPPKEYDeleter>&& pkey_ptr,
                    const string& scheme)
      : pkey(move(pkey_ptr)), scheme_name(scheme) {}
  virtual ~EVPPKEYPrivateKey() override {}

  virtual string Serialize() const override {
    if (EVP_PKEY_get0_EC_KEY(pkey.get())) {
      const EC_KEY* ec_key = EVP_PKEY_get0_EC_KEY(pkey.get());
      const BIGNUM* private_key = EC_KEY_get0_private_key(ec_key);
      if (!private_key) {
        throw UnexpectedOpenSSLCryptoFailureException(
            "OpenSSL Crypto unexpectedly failed to fetch the private key from "
            "an elliptic curve key pair.");
      }
      unique_ptr<char, OPENSSLStringDeleter> hex_chars(BN_bn2hex(private_key),
                                                       OPENSSLStringDeleter());
      if (!hex_chars) {
        throw UnexpectedOpenSSLCryptoFailureException(
            "OpenSSL Crypto unexpectedly failed to convert a private key to "
            "hexadecimal for serialization.");
      }
      string private_key_data = string(hex_chars.get());
      return "PRIVATE_KEY:" + scheme_name + ":" + private_key_data;
    } else {
      // This case should not be reachable as all currently supported schemes
      // should be handled above.
      throw invalid_argument(
          "Failed to serialize AsymmetricPrivateKey object; failed to identify "
          "supported key type; key serialization logic may be out of sync with "
          "key creation logic.");
    }
  }

  virtual std::string Sign(const std::string& message) const override {
    unique_ptr<EVP_MD_CTX, EVPMDCTXDeleter> digest_context(EVP_MD_CTX_new(),
                                                           EVPMDCTXDeleter());
    if (!digest_context) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to allocate a message digest "
          "context object.");
    }
    if (!EVP_DigestSignInit(digest_context.get(), nullptr, EVP_sha256(),
                            nullptr, pkey.get())) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to initialize a message digest "
          "context object.");
    }
    if (!EVP_DigestSignUpdate(digest_context.get(), message.data(),
                              message.length())) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to hash a message for signing.");
    }
    size_t signature_length;
    if (!EVP_DigestSignFinal(digest_context.get(), nullptr,
                             &signature_length)) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to determine the maximum length "
          "for a signature signed with asymmetric cryptography.");
    }
    string signature(signature_length, (char)0);
    if (!EVP_DigestSignFinal(digest_context.get(),
                             reinterpret_cast<unsigned char*>(signature.data()),
                             &signature_length)) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to sign a message digest with "
          "asymmetric cryptography.");
    }
    if (signature_length > signature.length()) {
      // This should probably never happen.
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly reports having produced an asymmetric "
          "cryptography signature of length greater than the maximum signature "
          "length it previously reported.");
    }
    // It is, at least theoretically, possible for the produced signature to be
    // shorter than the maximum signature length.
    signature.resize(signature_length);

    return signature;
  }
};

// Wrapper class for OpenSSL Crypto's EVP_PKEY objects implementing
// concord::utils::openssl_crypto::AsymmetricPublicKey.
class EVPPKEYPublicKey : public AsymmetricPublicKey {
 private:
  unique_ptr<EVP_PKEY, EVPPKEYDeleter> pkey;
  string scheme_name;

 public:
  // Copying and moving EVPPKEYPublicKey objects is currently unimplemented;
  // note the default implementations for copy and move operations would not be
  // appropriate for this class since the EVP_PKEY object it contains must be
  // memory-managed through the OpenSSL library rather than by default
  // mechanisms.
  EVPPKEYPublicKey(const EVPPKEYPublicKey& other) = delete;
  EVPPKEYPublicKey(const EVPPKEYPublicKey&& other) = delete;
  EVPPKEYPublicKey& operator=(const EVPPKEYPublicKey& other) = delete;
  EVPPKEYPublicKey& operator=(const EVPPKEYPublicKey&& other) = delete;

  // Note the constructed EVPPKEYPublicKey takes ownership of the pointer it is
  // constructed with. A precondition of this constructor is that the provided
  // EVP_PKEY object must its public key initialized; future behavior of this
  // EVPPKEYPublicKey object is undefined if this precondition is not met.
  EVPPKEYPublicKey(unique_ptr<EVP_PKEY, EVPPKEYDeleter>&& pkey_ptr,
                   const string& scheme)
      : pkey(move(pkey_ptr)), scheme_name(scheme) {}
  virtual ~EVPPKEYPublicKey() override {}

  virtual string Serialize() const override {
    if (EVP_PKEY_get0_EC_KEY(pkey.get())) {
      const EC_KEY* ec_key = EVP_PKEY_get0_EC_KEY(pkey.get());
      const EC_POINT* public_key = EC_KEY_get0_public_key(ec_key);
      if (!public_key) {
        throw UnexpectedOpenSSLCryptoFailureException(
            "OpenSSL Crypto unexpectedly failed to fetch the public key from "
            "an elliptic curve key object.");
      }
      const EC_GROUP* ec_group = EC_KEY_get0_group(ec_key);
      if (!ec_group) {
        throw UnexpectedOpenSSLCryptoFailureException(
            "OpenSSL Crypto unexpectedly failed to fetch the elliptic curve "
            "group from an elliptic curve key object.");
      }
      unique_ptr<BN_CTX, BNCTXDeleter> big_num_context(BN_CTX_new(),
                                                       BNCTXDeleter());
      if (!big_num_context) {
        throw UnexpectedOpenSSLCryptoFailureException(
            "OpenSSL Crypto unexpectedly failed to allocate a big number "
            "context object needed to convert an elliptic curve point to "
            "hexadecimal for serialization.");
      }
      unique_ptr<char, OPENSSLStringDeleter> hex_chars(
          EC_POINT_point2hex(ec_group, public_key,
                             EC_GROUP_get_point_conversion_form(ec_group),
                             big_num_context.get()),
          OPENSSLStringDeleter());
      if (!hex_chars) {
        throw UnexpectedOpenSSLCryptoFailureException(
            "OpenSSL Crypto unexpectedly failed to convert a public key to "
            "hexadecimal for serialization.");
      }
      string public_key_data = string(hex_chars.get());
      return "PUBLIC_KEY:" + scheme_name + ":" + public_key_data;
    } else {
      // This case should not be reachable as all currently supported schemes
      // should be handled above.
      throw invalid_argument(
          "Failed to serialize AsymmetricPublicKey object; failed to identify "
          "supported key type; key serialization logic may be out of sync with "
          "key creation logic.");
    }
  }

  virtual bool Verify(const std::string& message,
                      const std::string& signature) const override {
    unique_ptr<EVP_MD_CTX, EVPMDCTXDeleter> digest_context(EVP_MD_CTX_new(),
                                                           EVPMDCTXDeleter());
    if (!digest_context) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to allocate a message digest "
          "context object.");
    }
    if (!EVP_DigestVerifyInit(digest_context.get(), nullptr, EVP_sha256(),
                              nullptr, pkey.get())) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to initialize a message digest "
          "context object.");
    }
    if (!EVP_DigestVerifyUpdate(digest_context.get(), message.data(),
                                message.length())) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to hash a message to validate a "
          "signature over it.");
    }

    // Note we only return true if EVP_DigestVerifyFinal returns 1, and not if
    // it returns a non-1, non-0 value; only 1 indicates a verification success,
    // other non-1, non-0 values indicate unexpected failures (and 0 indicates
    // that the signature was not correct cryptographically).
    return (EVP_DigestVerifyFinal(
                digest_context.get(),
                reinterpret_cast<const unsigned char*>(signature.data()),
                signature.length()) == 1);
  }
};

pair<unique_ptr<AsymmetricPrivateKey>, unique_ptr<AsymmetricPublicKey>>
concord::utils::openssl_crypto::GenerateAsymmetricCryptoKeyPair(
    const string& scheme_name) {
  if (scheme_name == "secp256r1") {
    // prime256v1 is an alternative name for the same curve parameters as
    // secp256r1; prime256v1 happens to be the name OpenSSL's Crypto library
    // uses for a possible parameter to EC_KEY_new_by_curve_name.
    unique_ptr<EC_KEY, ECKEYDeleter> key_pair(
        EC_KEY_new_by_curve_name(NID_X9_62_prime256v1), ECKEYDeleter());
    unique_ptr<EC_KEY, ECKEYDeleter> public_key(
        EC_KEY_new_by_curve_name(NID_X9_62_prime256v1), ECKEYDeleter());
    if (!key_pair || !public_key) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to allocate and prepare an "
          "elliptic curve key object.");
    }
    if (!EC_KEY_generate_key(key_pair.get())) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to generate a new Elliptic Curve "
          "key pair.");
    }
    const EC_POINT* public_key_raw = EC_KEY_get0_public_key(key_pair.get());
    if (!public_key_raw) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to get a pointer to the public "
          "key for a generated elliptic curve key pair.");
    }
    if (!EC_KEY_set_public_key(public_key.get(), public_key_raw)) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to set the public key for an "
          "empty allocated elliptic curve key object.");
    }
    unique_ptr<EVP_PKEY, EVPPKEYDeleter> private_pkey(EVP_PKEY_new(),
                                                      EVPPKEYDeleter());
    unique_ptr<EVP_PKEY, EVPPKEYDeleter> public_pkey(EVP_PKEY_new(),
                                                     EVPPKEYDeleter());
    if (!private_pkey || !public_pkey) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to allocate and prepare a "
          "high-level key object.");
    }
    if (!EVP_PKEY_set1_EC_KEY(private_pkey.get(), key_pair.get()) ||
        !EVP_PKEY_set1_EC_KEY(public_pkey.get(), key_pair.get())) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to initialize a high-level key "
          "object given an elliptic curve key object.");
    }

    return pair<unique_ptr<AsymmetricPrivateKey>,
                unique_ptr<AsymmetricPublicKey>>(
        new EVPPKEYPrivateKey(move(private_pkey), scheme_name),
        new EVPPKEYPublicKey(move(public_pkey), scheme_name));
  } else {
    throw invalid_argument(
        "Cannot generate asymmetric cryptography key pair for cryptography "
        "scheme \"" +
        scheme_name +
        "\"; this scheme is either not recognized, not supported, or has not "
        "been permitted for use in Concord in consideration of our security "
        "requirements (which could be because it does not meet them, or could "
        "simply be because we have not assessed this particular scheme).");
  }
}

const string kLegalHexadecimalDigits = "0123456789ABCDEFabcdef";

unique_ptr<AsymmetricPrivateKey>
concord::utils::openssl_crypto::DeserializePrivateKey(const string& input) {
  size_t first_split_point = input.find(":");
  size_t second_split_point = input.rfind(":");
  if ((first_split_point == string::npos) ||
      (first_split_point == second_split_point)) {
    throw invalid_argument(
        "Failed to deserialize private key: input is malformatted and could "
        "not be parsed.");
  }
  string private_key_label = input.substr(0, first_split_point);
  string scheme_name = input.substr(first_split_point + 1,
                                    second_split_point - first_split_point - 1);
  string private_key_data = input.substr(second_split_point + 1);
  if (private_key_label != "PRIVATE_KEY") {
    throw invalid_argument(
        "Failed to deserialize private key: input does not appear to be "
        "labeled as a private key.");
  }
  if (scheme_name == "secp256r1") {
    if (private_key_data.find_first_not_of(kLegalHexadecimalDigits) !=
        string::npos) {
      throw invalid_argument(
          "Failed to deserialize private key of secp256r1 scheme: given key "
          "data (\"" +
          private_key_data + "\") is not purely hexadecimal.");
    }

    // prime256v1 is an alternative name for the same curve parameters as
    // secp256r1; prime256v1 happens to be the name OpenSSL's Crypto library
    // uses for a possible parameter to EC_KEY_new_by_curve_name.
    unique_ptr<EC_KEY, ECKEYDeleter> ec_key(
        EC_KEY_new_by_curve_name(NID_X9_62_prime256v1), ECKEYDeleter());
    if (!ec_key) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to allocate and prepare an "
          "elliptic curve key object.");
    }
    unique_ptr<BIGNUM, BIGNUMDeleter> private_key(nullptr, BIGNUMDeleter());
    BIGNUM* allocated_private_key = nullptr;
    int hex_parse_return_code =
        BN_hex2bn(&allocated_private_key, private_key_data.c_str());
    private_key.reset(allocated_private_key);
    if (!hex_parse_return_code) {
      throw invalid_argument(
          "Failed to deserialize private key of secp256r1 scheme: OpenSSL was "
          "unable to parse a private elliptic curve key from the private "
          "hexadecimal key data.");
    }
    if (!private_key) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to allocate a big number object "
          "and initialize it from hexadecimal data.");
    }
    if (!EC_KEY_set_private_key(ec_key.get(), private_key.get())) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to load the private key into an "
          "elliptic curve key object.");
    }

    // OpenSSL Crypto expects us to load the public key if the private key is
    // loaded, so we compute the public key from the private key here.
    const EC_GROUP* ec_group = EC_KEY_get0_group(ec_key.get());
    if (!ec_group) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to fetch the elliptic curve "
          "group for an elliptic curve key object.");
    }
    unique_ptr<EC_POINT, ECPOINTDeleter> public_key(EC_POINT_new(ec_group),
                                                    ECPOINTDeleter());
    if (!public_key) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to allocate an elliptic curve "
          "point object.");
    }
    unique_ptr<BN_CTX, BNCTXDeleter> public_key_derivation_context(
        BN_CTX_new(), BNCTXDeleter());
    if (!public_key_derivation_context) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to allocate a big number context "
          "object needed to derive a public elliptic curve key from the "
          "corresponding private key.");
    }
    if (!EC_POINT_mul(ec_group, public_key.get(), private_key.get(), nullptr,
                      nullptr, public_key_derivation_context.get())) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to derive a public elliptic "
          "curve key from the corresponding private key.");
    }
    if (!EC_KEY_set_public_key(ec_key.get(), public_key.get())) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to a public key to an elliptic "
          "curve key object.");
    }

    unique_ptr<EVP_PKEY, EVPPKEYDeleter> private_pkey(EVP_PKEY_new(),
                                                      EVPPKEYDeleter());
    if (!private_pkey) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to allocate and prepare a "
          "high-level key object.");
    }
    if (!EVP_PKEY_set1_EC_KEY(private_pkey.get(), ec_key.get())) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to initialize a high-level key "
          "object given an elliptic curve key object.");
    }

    return unique_ptr<AsymmetricPrivateKey>(
        new EVPPKEYPrivateKey(move(private_pkey), scheme_name));
  } else {
    throw invalid_argument(
        "Failed to deserialize private key: scheme \"" + scheme_name +
        "\" is either not recognized, not supported, or has not been permitted "
        "for use in Concord in consideration of our security requirements "
        "(which could be because it does not meet them, or could simply be "
        "because we have not assessed this particular scheme).");
  }
}

unique_ptr<AsymmetricPublicKey>
concord::utils::openssl_crypto::DeserializePublicKey(const string& input) {
  size_t first_split_point = input.find(":");
  size_t second_split_point = input.rfind(":");
  if ((first_split_point == string::npos) ||
      (first_split_point == second_split_point)) {
    throw invalid_argument(
        "Failed to deserialize public key: input is malformatted and could not "
        "be parsed.");
  }
  string public_key_label = input.substr(0, first_split_point);
  string scheme_name = input.substr(first_split_point + 1,
                                    second_split_point - first_split_point - 1);
  string public_key_data = input.substr(second_split_point + 1);
  if (public_key_label != "PUBLIC_KEY") {
    throw invalid_argument(
        "Failed to deserialize public key: input does not appear to be labeled "
        "as a public key.");
  }
  if (scheme_name == "secp256r1") {
    if (public_key_data.find_first_not_of(kLegalHexadecimalDigits) !=
        string::npos) {
      throw invalid_argument(
          "Failed to deserialize public key of secp256r1 scheme: given key "
          "data (\"" +
          public_key_data + "\") is not purely hexadecimal.");
    }

    // prime256v1 is an alternative name for the same curve parameters as
    // secp256r1; prime256v1 happens to be the name OpenSSL's Crypto library
    // uses for a possible parameter to EC_KEY_new_by_curve_name.
    unique_ptr<EC_KEY, ECKEYDeleter> ec_key(
        EC_KEY_new_by_curve_name(NID_X9_62_prime256v1), ECKEYDeleter());
    if (!ec_key) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to allocate and prepare an "
          "elliptic curve key object.");
    }
    const EC_GROUP* ec_group = EC_KEY_get0_group(ec_key.get());
    if (!ec_group) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to fetch the elliptic curve "
          "group from an elliptic curve key object.");
    }
    unique_ptr<EC_POINT, ECPOINTDeleter> public_key(EC_POINT_new(ec_group),
                                                    ECPOINTDeleter());
    if (!public_key) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to allocate an elliptic curve "
          "point object.");
    }
    unique_ptr<BN_CTX, BNCTXDeleter> big_num_context(BN_CTX_new(),
                                                     BNCTXDeleter());
    if (!big_num_context) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to allocate a big number context "
          "object needed to convert hexadecimal data to an elliptic cuve point "
          "for deserialization.");
    }
    if (!EC_POINT_hex2point(ec_group, public_key_data.c_str(), public_key.get(),
                            big_num_context.get())) {
      throw invalid_argument(
          "Failed to deserialize public key of secp256r1 scheme: OpenSSL "
          "failed to parse an elliptic curve private key from hexadecimal "
          "data..");
    }
    if (!EC_KEY_set_public_key(ec_key.get(), public_key.get())) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to load the public key to an "
          "elliptic curve key object.");
    }

    unique_ptr<EVP_PKEY, EVPPKEYDeleter> public_pkey(EVP_PKEY_new(),
                                                     EVPPKEYDeleter());
    if (!public_pkey) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to allocate and prepare a "
          "high-level key object.");
    }
    if (!EVP_PKEY_set1_EC_KEY(public_pkey.get(), ec_key.get())) {
      throw UnexpectedOpenSSLCryptoFailureException(
          "OpenSSL Crypto unexpectedly failed to initialize a high-level key "
          "object given an elliptic curve key object.");
    }

    return unique_ptr<AsymmetricPublicKey>(
        new EVPPKEYPublicKey(move(public_pkey), scheme_name));
  } else {
    throw invalid_argument(
        "Failed to deserialize public key: scheme \"" + scheme_name +
        "\" is either not recognized, not supported, or has not been permitted "
        "for use in Concord in consideration of our security requirements "
        "(which could be because it does not meet them, or could simply be "
        "because we have not assessed this particular scheme).");
  }
}
