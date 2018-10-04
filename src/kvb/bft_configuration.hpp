// Copyright 2018 VMware, all rights reserved
//
// Temporary solution for creating configuration structs for concord-bft
// Ideally, the ReplicaConfig from BlockchainInterfaces.h

#ifndef ATHENA_CONFIG_PARSER_HPP
#define ATHENA_CONFIG_PARSER_HPP

#include "BlockchainInterfaces.h"
#include "IThresholdSigner.h"
#include "IThresholdVerifier.h"
#include "IThresholdFactory.h"
#include <string>
#include <set>

namespace com {
namespace vmware {
namespace athena {

const size_t MAX_ITEM_LENGTH = 4096;
const std::string MAX_ITEM_LENGTH_STR = std::to_string(MAX_ITEM_LENGTH);

inline void
read_one_line(FILE *f, int capacity, char *buf) {
  // Read next line
  fgets(buf, capacity - 1, f);
  buf[strlen(buf) - 1] = 0; // strip newline
}

inline void
ignore_one_line(FILE *f) {
  char buf[8192];
  read_one_line(f, 8191, buf);
}

inline IThresholdFactory *
create_threshold_factory(FILE *config_file) {
  using namespace BLS::Relic;

  char buf[4096];
  char *curveType = buf;
  char cryptosys[4096];

  // Ignore next line (has comment about threshold type)
  ignore_one_line(config_file);
  // Read cryptosystem type
  read_one_line(config_file, 4096, cryptosys);
  // Reads the RSA modulus or elliptic curve type
  read_one_line(config_file, 4096, buf);

  if (strcmp(cryptosys, MULTISIG_BLS_SCHEME) == 0) {
    return new BlsThresholdFactory(PublicParametersFactory::
                                   getByCurveType(curveType));
  } else if (strcmp(cryptosys, THRESHOLD_BLS_SCHEME) == 0) {
    return new BlsThresholdFactory(PublicParametersFactory::
                                   getByCurveType(curveType));
  } else {
    return nullptr;
  }
}

inline IThresholdSigner *
create_threshold_signer(FILE *config_priv, int id, IThresholdFactory *factory) {
  if (id < 1) {
    return nullptr;
  }

  char secretKey[4096];

  // Ignore next line (has comment about threshold type)
  ignore_one_line(config_priv);

  // Read SK
  read_one_line(config_priv, 4096, secretKey);
  return factory->newSigner(id, secretKey);
}

inline IThresholdVerifier *
create_threshold_verifier(FILE *config_file,
                          int requestedSigners,
                          int totalSigners,
                          IThresholdFactory *factory) {
  char publicKey[4096];
  char verifKey[4096];
  //cout << "Reading verifier..." << endl;

  // NOTE: For RSA this is the public exponent only (no modulus N)
  read_one_line(config_file, 4096, publicKey);

  // Read share verification keys from file
  // (even if not implemented, they should be dummy lines)
  // First, read the dummy line
  ignore_one_line(config_file);
  std::vector<std::string> verifKeys;
  verifKeys.push_back("");    // signer 0 does not exist
  for (int i = 0; i < totalSigners; i++) {
    read_one_line(config_file, 4096, verifKey);
    //cout << "Read share verif key '" << verifKey << "'" << endl;
    verifKeys.push_back(std::string(verifKey));
  }

  // Ignore last comment/empty line
  ignore_one_line(config_file);

  return factory->newVerifier(requestedSigners,
                              totalSigners,
                              publicKey,
                              verifKeys);
}

static void
ignore_threshold_signer(FILE *config_priv) {
  //cout << "Ignoring signer..." << endl;
  // Ignore next line (has comment about threshold type)
  ignore_one_line(config_priv);

  // Ignore SK
  ignore_one_line(config_priv);
}

static void
ignore_threshold_factory_and_verifier(FILE *config_file, int n) {
  //cout << "Ignoring threshold scheme..." << endl;
  // Ignore next line (has comment about threshold type)
  ignore_one_line(config_file);
  // Ignore cryptosystem type
  ignore_one_line(config_file);
  // Ignore the RSA modulus or elliptic curve type
  ignore_one_line(config_file);
  // Ignore PK
  ignore_one_line(config_file);
  // Ignore comment
  ignore_one_line(config_file);
  // Ignore VKs
  for (int i = 0; i < n; i++) {
    ignore_one_line(config_file);
  }
  // Ignore last comment/empty line
  ignore_one_line(config_file);
}

void read_threshold_public_keys(
    FILE *config_file,
    bool isClient,
    int f,
    int c,
    bool supportDirectProofs,
    IThresholdFactory *&execFactory,
    IThresholdFactory *&slowCommitFactory,
    IThresholdFactory *&commitFactory,
    IThresholdFactory *&multisigFactory,
    IThresholdVerifier *&thresholdVerifierForExecution,
    IThresholdVerifier *&thresholdVerifierForSlowPathCommit,
    IThresholdVerifier *&thresholdVerifierForCommit,
    IThresholdVerifier *&thresholdVerifierForOptimisticCommit) {

  int n = 3 * f + 2 * c + 1;

  if (supportDirectProofs) {
    execFactory = create_threshold_factory(config_file);
    thresholdVerifierForExecution = create_threshold_verifier(config_file,
                                                              f + 1, n,
                                                              execFactory);
  } else {
    ignore_threshold_factory_and_verifier(config_file, n);
  }

  // The Client class only needs the f+1 parameters
  if (isClient) {
    return;
  }

  slowCommitFactory = create_threshold_factory(config_file);
  thresholdVerifierForSlowPathCommit = create_threshold_verifier(config_file,
                                                                 2 * f + c + 1,
                                                                 n,
                                                                 slowCommitFactory);

  if (c > 0) {
    commitFactory = create_threshold_factory(config_file);
    thresholdVerifierForCommit = create_threshold_verifier(config_file,
                                                           3 * f + c + 1, n,
                                                           commitFactory);
  } else {
    ignore_threshold_factory_and_verifier(config_file, n);
  }

  multisigFactory = create_threshold_factory(config_file);
  thresholdVerifierForOptimisticCommit = create_threshold_verifier(config_file,
                                                                   n,
                                                                   n,
                                                                   multisigFactory);
}

/*
 * Reads the secret keys for the multisig and threshold schemes!
 */
void
read_threshold_private_keys(
    FILE *config_priv,
    int myReplicaId,
    int f,
    int c,
    IThresholdFactory *execFactory,
    IThresholdFactory *slowCommitFactory,
    IThresholdFactory *commitFactory,
    IThresholdFactory *multisigFactory,
    IThresholdSigner *&thresholdSignerForExecution,
    IThresholdSigner *&thresholdSignerForSlowPathCommit,
    IThresholdSigner *&thresholdSignerForCommit,
    IThresholdSigner *&thresholdSignerForOptimisticCommit,
    bool supportDirectProofs) {
  // f + 1
  if (supportDirectProofs) {
    thresholdSignerForExecution =
        create_threshold_signer(
            config_priv,
            myReplicaId,
            execFactory);
  } else {
    printf("\n does not support direct proofs!");
    ignore_threshold_signer(config_priv);
  }

  // 2f + c + 1
  thresholdSignerForSlowPathCommit =
      create_threshold_signer(config_priv,
                              myReplicaId,
                              slowCommitFactory);

  // 3f + c + 1
  if (c > 0) {
    thresholdSignerForCommit = create_threshold_signer(config_priv,
                                                       myReplicaId,
                                                       commitFactory);
  } else {
    printf("\n c <= 0");
    ignore_threshold_signer(config_priv);
  }

  // Reading multisig secret keys for the case where everybody sign case where everybody signs
  thresholdSignerForOptimisticCommit = create_threshold_signer(config_priv,
                                                               myReplicaId,
                                                               multisigFactory);
}

inline bool
parse_crypto(uint16_t nodeId,
             uint16_t numOfReplicas,
             uint16_t maxFaulty,
             uint16_t maxSlow,
             FILE *publicKeysFile,
             FILE *privateKeysFile,
             std::set<pair<uint16_t, string>> publicKeysOfReplicas,
             Blockchain::ReplicaConsensusConfig *outConfig) {
  // Read threshold signatures
  // Threshold signatures
  IThresholdFactory *execFactory;
  IThresholdSigner *thresholdSignerForExecution;
  IThresholdVerifier *thresholdVerifierForExecution;

  IThresholdFactory *slowCommitFactory;
  IThresholdSigner *thresholdSignerForSlowPathCommit;
  IThresholdVerifier *thresholdVerifierForSlowPathCommit;

  IThresholdFactory *commitFactory;
  IThresholdSigner *thresholdSignerForCommit;
  IThresholdVerifier *thresholdVerifierForCommit;

  IThresholdFactory *multisigFactory;
  IThresholdSigner *thresholdSignerForOptimisticCommit;
  IThresholdVerifier *thresholdVerifierForOptimisticCommit;

  /// TODO(IG): move to config
  const bool supportDirectProofs = false;

  read_threshold_public_keys(publicKeysFile,
                             false,
                             maxFaulty,
                             maxSlow,
                             supportDirectProofs,
                             execFactory,
                             slowCommitFactory,
                             commitFactory,
                             multisigFactory,
                             thresholdVerifierForExecution,
                             thresholdVerifierForSlowPathCommit,
                             thresholdVerifierForCommit,
                             thresholdVerifierForOptimisticCommit);

  // Read threshold secret keys from private config
  read_threshold_private_keys(privateKeysFile,
                              nodeId + 1,
                              maxFaulty,
                              maxSlow,
                              execFactory,
                              slowCommitFactory,
                              commitFactory,
                              multisigFactory,
                              thresholdSignerForExecution,
                              thresholdSignerForSlowPathCommit,
                              thresholdSignerForCommit,
                              thresholdSignerForOptimisticCommit,
                              supportDirectProofs);

  outConfig->publicKeysOfReplicas = publicKeysOfReplicas;

  outConfig->thresholdSignerForExecution = nullptr;
  outConfig->thresholdVerifierForExecution = nullptr;

  outConfig->thresholdSignerForSlowPathCommit =
      thresholdSignerForSlowPathCommit;
  outConfig->thresholdVerifierForSlowPathCommit =
      thresholdVerifierForSlowPathCommit;

  outConfig->thresholdSignerForCommit = nullptr;
  outConfig->thresholdVerifierForCommit = nullptr;

  outConfig->thresholdSignerForOptimisticCommit =
      thresholdSignerForOptimisticCommit;
  outConfig->thresholdVerifierForOptimisticCommit =
      thresholdVerifierForOptimisticCommit;

  return true;
}

inline bool
parse_principals(FILE *config_file,
                 uint16_t selfNumber,
                 uint16_t numOfPrincipals,
                 uint16_t numOfReplicas,
                 Blockchain::CommConfig *outCommConfig,
                 std::set<std::pair<uint16_t, std::string>>
                    &outReplicasPublicKeys) {
  //skip group ip and port
  fscanf(config_file, "%*[^\n]\n");

  // read in all the principals
  char addr_buff[100];
  char host_name[100];// not in use
  uint16_t port;
  char pk[4096];
  for (int i = 0; i < numOfPrincipals; i++) {
    bool isReplica = false;
    fscanf(config_file,
           "%256s %32s %hd\n%4096s\n",
           host_name,
           addr_buff,
           &port,
           pk);
    if (i < numOfReplicas) {
      isReplica = true;
      outReplicasPublicKeys.insert({i, std::string(pk)});
    }
    if (outCommConfig) {
      outCommConfig->nodes.insert({i, NodeInfo{string(addr_buff),
                                               port,
                                               isReplica}});
      if (i == selfNumber) {
        outCommConfig->listenPort = port;
      }
    }
  }

  if (outCommConfig) {
    /// TODO(IG): add to config file
    outCommConfig->bufferLength = 64000;
    outCommConfig->listenIp = "0.0.0.0";
    outCommConfig->maxServerId = numOfReplicas - 1;
    outCommConfig->selfId = selfNumber;
    outCommConfig->certificatesRootPath = "\\certs";
  }

  return true;
}

inline bool
parse_plain_config_file(std::string privPath,
                        std::string pubPath,
                        Blockchain::CommConfig *commConfig,
                        Blockchain::ClientConsensusConfig *clConf,
                        Blockchain::ReplicaConsensusConfig *repConf) {
  assert(!clConf != !repConf);
  FILE *config_file = fopen(pubPath.c_str(), "r");
  if (!config_file) {
    return false;
  }

  FILE *config_priv_file = fopen(privPath.c_str(), "r");
  if (!config_priv_file) {
    return false;
  }

  //
  // Initialize random number generator
  srand48(getpid());

  uint16_t selfNumber;
  fscanf(config_priv_file, "%hu\n", &selfNumber);

  // secret key
  char sk1[4096];
  fscanf(config_priv_file, "%s\n", sk1);

  //skip first line
  fscanf(config_file, "%*[^\n]\n");

  // read max_faulty
  uint16_t max_faulty = 0;
  fscanf(config_file, "%hu\n", &max_faulty);

  // read max_slow
  uint16_t max_slow = 0;
  fscanf(config_file, "%hu\n", &max_slow);

  //skip timeout
  fscanf(config_file, "%*[^\n]\n");

  // read num of principals
  uint16_t numOfPrincipals = 0;
  fscanf(config_file, "%hu\n", &numOfPrincipals);

  uint16_t numOfReplicas = 3 * max_faulty + 2 * max_slow + 1;

  std::set<pair<uint16_t, string>> publicKeysOfReplicas;
  if (commConfig) {
    auto res = parse_principals(config_file,
                                selfNumber,
                                numOfPrincipals,
                                numOfReplicas,
                                commConfig,
                                publicKeysOfReplicas);
    if (!res)
      return false;
  }

  if (repConf) {
    repConf->replicaPrivateKey = std::string(sk1);

    //skip empty line
    // fscanf(config_file, "%*[^\n]\n", NULL);
    auto res = parse_crypto(
        selfNumber,
        numOfReplicas,
        max_faulty,
        max_slow,
        config_file,
        config_priv_file,
        publicKeysOfReplicas,
        repConf);
    if (!res)
      return false;

    uint16_t viewChangeTimeoutMilli, statusTimeMilli, concurrencyLevel;
    fscanf(config_file, "%hu\n", &viewChangeTimeoutMilli);
    fscanf(config_file, "%hu\n", &statusTimeMilli);
    //skip one line
    fscanf(config_file, "%*[^\n]\n");
    fscanf(config_file, "%hu\n", &concurrencyLevel);

    repConf->publicKeysOfReplicas = publicKeysOfReplicas;
    repConf->viewChangeTimerMillisec = viewChangeTimeoutMilli;
    repConf->statusReportTimerMillisec = statusTimeMilli;
    repConf->concurrencyLevel = concurrencyLevel;

    repConf->replicaId = selfNumber;
    repConf->fVal = max_faulty;
    repConf->cVal = max_slow;
    repConf->numOfClientProxies = numOfPrincipals - numOfReplicas;

    //TODO(IG): add to config file
    repConf->autoViewChangeEnabled = true;

  } else {
    clConf->clientId = selfNumber;
    clConf->maxFaulty = max_faulty;
    clConf->maxSlow = max_slow;
  }

  return true;
}

}
}
}

#endif //ATHENA_CONFIG_PARSER_HPP
