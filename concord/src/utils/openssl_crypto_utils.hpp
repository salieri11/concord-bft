// Copyright 2020 VMware, all rights reserved.
//
// Thin wrapper over selected cryptographic function utilities from OpenSSL's
// crypto library, which we consider a reasonably trusted source of
// cryptographic implementations. This wrapper is intended to provide a cleaner
// and more convenient interface to the OpenSSL crypto library to fit better
// with the rest of the Concord codebase, as the OpenSSL crypto library itself
// has a C interface.

#ifndef UTILS_OPENSSL_CRYPTO_HPP
#define UTILS_OPENSSL_CRYPTO_HPP

#include <memory>
#include <ostream>
#include <vector>

namespace concord {
namespace utils {
namespace openssl_crypto {

// Note these utilities may use std::strings to pass arond byte strings; note
// this should work since C++ should guarantee std::string is a string of chars
// specifically and that the char type is exactly 1 byte in size.

// Interface for wrapper class for private keys in asymmetric cryptography
// schemes.
class AsymmetricPrivateKey {
 public:
  // As this is an abstract class written with the intention it should be used
  // only through polymorphic pointers, copying or moving AsymmetricPrivateKey
  // instances directly is not supported.
  AsymmetricPrivateKey(const AsymmetricPrivateKey& other) = delete;
  AsymmetricPrivateKey(const AsymmetricPrivateKey&& other) = delete;
  AsymmetricPrivateKey& operator=(const AsymmetricPrivateKey& other) = delete;
  AsymmetricPrivateKey& operator=(const AsymmetricPrivateKey&& other) = delete;

  virtual ~AsymmetricPrivateKey() {}

  // Serialize the private key to a printable format, returning the serialized
  // object as a string. May throw an UnexpectedOpenSSLCryptoFailureException if
  // the underlying OpenSSL Crypto library unexpectedly reports a failure while
  // attempting this operation.
  virtual std::string Serialize() const = 0;

  // Produce and return a signature of a given message. Note both the message
  // and signature are logically byte strings handled via std::strings. May
  // throw an UnexpectedOpenSSLCryptoFailureException if the underlying OpenSSL
  // Crypto library unexpectedly reports a failure while attempting this
  // operation.
  virtual std::string Sign(const std::string& message) const = 0;

 protected:
  AsymmetricPrivateKey() {}
};

// Interface for wrapper classes for public keys in asymmetric cryptography
// schemes.
class AsymmetricPublicKey {
 public:
  // As this is an abstract class written with the intention it should be used
  // only through polymorphic pointers, copying or moving AsymmetricPublicKey
  // instances directly is not supported.
  AsymmetricPublicKey(const AsymmetricPublicKey& other) = delete;
  AsymmetricPublicKey(const AsymmetricPublicKey&& other) = delete;
  AsymmetricPublicKey& operator=(const AsymmetricPublicKey& other) = delete;
  AsymmetricPublicKey& operator=(const AsymmetricPublicKey&& other) = delete;

  virtual ~AsymmetricPublicKey() {}

  // Serialize the public key to a printable format, returning the serialized
  // object as a string. May throw an UnexpectedOpenSSLCryptoFailureException if
  // the underlying OpenSSL Crypto library unexpectedly reports a failure while
  // attempting this operation.
  virtual std::string Serialize() const = 0;

  // Verify a signature of a message allegedly signed with the private key
  // corresponding to this public key. Note both the message and alleged
  // signature are logically byte strings handled via std::strings. Returns
  // true if this private key validates the signature as being a valid signature
  // of the message under the corresponding public key, and false otherwise. May
  // throw an UnexpectedOpenSSLCryptoFailureException if the underlying OpenSSL
  // Crypto library unexpectedly reports a failure while attempting this
  // operation.
  virtual bool Verify(const std::string& message,
                      const std::string& signature) const = 0;

 protected:
  AsymmetricPublicKey() {}
};

// List of currently permitted and supported asymmetric cryptography schemes
// Concord might use. Note Concord maintainers should not add new schemes to
// this list and implement support for them unless those new schemes have been
// vetted for acceptability and adequacy for our purposes, considering our
// security requirements. May throw an UnexpectedOpenSSLCryptoFailureException
// if the underlying OpenSSL Crypto library unexpectedly reports a failure while
// attempting this operation.
const static std::vector<std::string> kPermittedAsymmetricCryptoSchemes({
    "secp256r1"  // secp256r1 is an ECDSA scheme. Apparently it is
                 // NIST-approved.
                 // TODO (Alex): (I am using this curve as it was recommended by
                 //              Ittai) Find reputable sources vouching for the
                 //              security of this curve and cite them here.
                 // TODO (Alex): Also consider seeing if VMware's security
                 //              department has any comments on the security of
                 //              this curve.
});

// Function for pseudorandomly generating a key pair for asymmetric
// cryptography, given an asymmetric cryptography scheme listed in
// KPermittedAsymmetricCryptoSchemes.
// Throws an std::invalid_argument if the scheme_name parameter is not permitted
// and supported (that is, if scheme_name is not listed in
// kPermittedAsymmetricCryptoSchemes). May throw an
// UnexpectedOpenSSLCryptoFailureException if the underlying OpenSSL Crypto
// library unexpectedly reports a failure while attempting this operation.
std::pair<std::unique_ptr<AsymmetricPrivateKey>,
          std::unique_ptr<AsymmetricPublicKey>>
GenerateAsymmetricCryptoKeyPair(const std::string& scheme_name);

// Deserialize a private key serialized with an implementation of
// AsymmetricPrivateKey's implementation of Serialize from a given string.
// This function will handle determining what scheme the key is for and
// constructing an AsymmetricPrivateKey object of the appropriate type. This
// function may throw an invalid_argument exception if it fails to parse the
// serialized key. It may also throw an UnexpectedOpenSSLCryptoFailureException
// if the underlying OpenSSL Crypto library unexpectedly reports a failure while
// attempting this operation.
std::unique_ptr<AsymmetricPrivateKey> DeserializePrivateKey(
    const std::string& input);

// Deserialize a public key serialized with an implementation of
// AsymmetricPublicKey's implementation of Serialize from a given string.
// This function will handle determining what scheme the key is for and
// constructing an AsymmetricPublicKey object of the appropriate type. This
// function may throw an invalid_argument exception if it fails to parse the
// serialized key. It may also throw an UnexpectedOpenSSLCryptoFailureException
// if the underlying OpenSSL Crypto library unexpectedly reports a failure while
// attempting this operation.
std::unique_ptr<AsymmetricPublicKey> DeserializePublicKey(
    const std::string& input);

// Specialized Exception types that may be thrown by the above utilities.

// Exception that may be thrown if a call into OpenSSLCrypto returns a failure
// unexpectedly.
class UnexpectedOpenSSLCryptoFailureException : public std::exception {
 private:
  std::string message;

 public:
  explicit UnexpectedOpenSSLCryptoFailureException(const std::string& what)
      : message(what) {}
  virtual const char* what() const noexcept override { return message.c_str(); }
};

}  // namespace openssl_crypto
}  // namespace utils
}  // namespace concord

#endif  // UTILS_OPENSSL_CRYPTO_HPP
