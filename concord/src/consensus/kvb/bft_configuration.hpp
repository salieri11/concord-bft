// Copyright 2018-2019 VMware, all rights reserved
//
// Temporary solution for creating configuration structs for concord-bft
// Ideally, the ReplicaConfig from BlockchainInterfaces.h

#ifndef CONCORD_CONFIG_PARSER_HPP
#define CONCORD_CONFIG_PARSER_HPP

#include <set>
#include <string>
#include "BlockchainInterfaces.h"
#include "IThresholdFactory.h"
#include "IThresholdSigner.h"
#include "IThresholdVerifier.h"
#include "config/configuration_manager.hpp"

using concord::config::ConcordConfiguration;
using concord::config::ConcordPrimaryConfigurationAuxiliaryState;

namespace com {
namespace vmware {
namespace concord {

const size_t MAX_ITEM_LENGTH = 4096;
const std::string MAX_ITEM_LENGTH_STR = std::to_string(MAX_ITEM_LENGTH);

void initializeSBFTThresholdPublicKeys(
    ConcordConfiguration& config, bool isClient, uint16_t f, uint16_t c,
    bool supportDirectProofs,
    IThresholdVerifier*& thresholdVerifierForExecution,
    IThresholdVerifier*& thresholdVerifierForSlowPathCommit,
    IThresholdVerifier*& thresholdVerifierForCommit,
    IThresholdVerifier*& thresholdVerifierForOptimisticCommit) {
  ConcordPrimaryConfigurationAuxiliaryState* auxState;
  assert(auxState = dynamic_cast<ConcordPrimaryConfigurationAuxiliaryState*>(
             config.getAuxiliaryState()));

  if (supportDirectProofs) {
    assert(auxState->executionCryptosys);
    thresholdVerifierForExecution =
        auxState->executionCryptosys->createThresholdVerifier();
  }

  // The Client class only needs the f+1 parameters
  if (isClient) {
    return;
  }

  assert(auxState->slowCommitCryptosys);
  thresholdVerifierForSlowPathCommit =
      auxState->slowCommitCryptosys->createThresholdVerifier();

  if (c > 0) {
    assert(auxState->commitCryptosys);
    thresholdVerifierForCommit =
        auxState->commitCryptosys->createThresholdVerifier();
  }

  assert(auxState->optimisticCommitCryptosys);
  thresholdVerifierForOptimisticCommit =
      auxState->optimisticCommitCryptosys->createThresholdVerifier();
}

/*
 * Reads the secret keys for the multisig and threshold schemes!
 */
void initializeSBFTThresholdPrivateKeys(
    ConcordConfiguration& config, uint16_t myReplicaId, uint16_t f, uint16_t c,
    IThresholdSigner*& thresholdSignerForExecution,
    IThresholdSigner*& thresholdSignerForSlowPathCommit,
    IThresholdSigner*& thresholdSignerForCommit,
    IThresholdSigner*& thresholdSignerForOptimisticCommit,
    bool supportDirectProofs) {
  ConcordPrimaryConfigurationAuxiliaryState* auxState;
  assert(auxState = dynamic_cast<ConcordPrimaryConfigurationAuxiliaryState*>(
             config.getAuxiliaryState()));

  // f + 1
  if (supportDirectProofs) {
    assert(auxState->executionCryptosys);
    thresholdSignerForExecution =
        auxState->executionCryptosys->createThresholdSigner();
  } else {
    printf("\n does not support direct proofs!");
  }

  // 2f + c + 1
  assert(auxState->slowCommitCryptosys);
  thresholdSignerForSlowPathCommit =
      auxState->slowCommitCryptosys->createThresholdSigner();

  // 3f + c + 1
  if (c > 0) {
    assert(auxState->commitCryptosys);
    thresholdSignerForCommit =
        auxState->commitCryptosys->createThresholdSigner();
  } else {
    printf("\n c <= 0");
  }

  // Reading multisig secret keys for the case where everybody sign case where
  // everybody signs
  assert(auxState->optimisticCommitCryptosys);
  thresholdSignerForOptimisticCommit =
      auxState->optimisticCommitCryptosys->createThresholdSigner();
}

inline bool initializeSBFTCrypto(
    uint16_t nodeId, uint16_t numOfReplicas, uint16_t maxFaulty,
    uint16_t maxSlow, ConcordConfiguration& config,
    ConcordConfiguration& replicaConfig,
    std::set<std::pair<uint16_t, std::string>> publicKeysOfReplicas,
    Blockchain::ReplicaConsensusConfig* outConfig) {
  // Threshold signatures
  IThresholdSigner* thresholdSignerForExecution;
  IThresholdVerifier* thresholdVerifierForExecution;

  IThresholdSigner* thresholdSignerForSlowPathCommit;
  IThresholdVerifier* thresholdVerifierForSlowPathCommit;

  IThresholdSigner* thresholdSignerForCommit;
  IThresholdVerifier* thresholdVerifierForCommit;

  IThresholdSigner* thresholdSignerForOptimisticCommit;
  IThresholdVerifier* thresholdVerifierForOptimisticCommit;

  /// TODO(IG): move to config
  const bool supportDirectProofs = false;

  initializeSBFTThresholdPublicKeys(
      config, false, maxFaulty, maxSlow, supportDirectProofs,
      thresholdVerifierForExecution, thresholdVerifierForSlowPathCommit,
      thresholdVerifierForCommit, thresholdVerifierForOptimisticCommit);

  initializeSBFTThresholdPrivateKeys(
      config, nodeId + 1, maxFaulty, maxSlow, thresholdSignerForExecution,
      thresholdSignerForSlowPathCommit, thresholdSignerForCommit,
      thresholdSignerForOptimisticCommit, supportDirectProofs);

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

inline bool initializeSBFTPrincipals(
    ConcordConfiguration& config, uint16_t selfNumber, uint16_t numOfPrincipals,
    uint16_t numOfReplicas, Blockchain::CommConfig* outCommConfig,
    std::set<std::pair<uint16_t, std::string>>& outReplicasPublicKeys) {
  uint16_t clientProxiesPerReplica =
      config.getValue<uint16_t>("client_proxies_per_replica");
  for (uint16_t i = 0; i < numOfReplicas; ++i) {
    ConcordConfiguration& nodeConfig = config.subscope("node", i);
    ConcordConfiguration& replicaConfig = nodeConfig.subscope("replica", 0);
    uint16_t replicaId = replicaConfig.getValue<uint16_t>("principal_id");
    uint16_t replicaPort = replicaConfig.getValue<uint16_t>("replica_port");
    outReplicasPublicKeys.insert(
        {replicaId, replicaConfig.getValue<std::string>("public_key")});
    if (outCommConfig) {
      outCommConfig->nodes.insert(
          {replicaId,
           NodeInfo{replicaConfig.getValue<std::string>("replica_host"),
                    replicaPort, true}});
      if (replicaId == selfNumber) {
        outCommConfig->listenPort = replicaPort;
      }
      for (uint16_t j = 0; j < clientProxiesPerReplica; ++j) {
        ConcordConfiguration& clientConfig =
            nodeConfig.subscope("client_proxy", j);
        uint16_t clientId = clientConfig.getValue<uint16_t>("principal_id");
        uint16_t clientPort = clientConfig.getValue<uint16_t>("client_port");
        outCommConfig->nodes.insert(
            {clientId,
             NodeInfo{clientConfig.getValue<std::string>("client_host"),
                      clientPort, false}});
        if (clientId == selfNumber) {
          outCommConfig->listenPort = clientPort;
        }
      }
    }
  }

  if (outCommConfig) {
    /// TODO(IG): add to config file
    outCommConfig->bufferLength = 64000;
    outCommConfig->listenIp = "0.0.0.0";
    outCommConfig->maxServerId = numOfReplicas - 1;
    outCommConfig->selfId = selfNumber;
    outCommConfig->cipherSuite =
        config.getValue<std::string>("tls_cipher_suite_list");
    outCommConfig->certificatesRootPath =
        config.getValue<std::string>("tls_certificates_folder_path");
    outCommConfig->commType = config.getValue<std::string>("comm_to_use");
  }

  return true;
}

inline bool initializeSBFTConfiguration(
    ConcordConfiguration& config, ConcordConfiguration& nodeConfig,
    Blockchain::CommConfig* commConfig,
    Blockchain::ClientConsensusConfig* clConf, uint16_t clientIndex,
    Blockchain::ReplicaConsensusConfig* repConf) {
  assert(!clConf != !repConf);

  // Initialize random number generator
  srand48(getpid());

  ConcordConfiguration& replicaConfig = nodeConfig.subscope("replica", 0);
  uint16_t selfNumber = (repConf)
                            ? (replicaConfig.getValue<uint16_t>("principal_id"))
                            : (nodeConfig.subscope("client_proxy", clientIndex)
                                   .getValue<uint16_t>("principal_id"));
  uint16_t maxFaulty = config.getValue<uint16_t>("f_val");
  uint16_t maxSlow = config.getValue<uint16_t>("c_val");
  uint16_t numOfPrincipals = config.getValue<uint16_t>("num_principals");
  uint16_t numOfReplicas = config.getValue<uint16_t>("num_replicas");

  std::set<pair<uint16_t, string>> publicKeysOfReplicas;
  if (commConfig) {
    bool res = initializeSBFTPrincipals(config, selfNumber, numOfPrincipals,
                                        numOfReplicas, commConfig,
                                        publicKeysOfReplicas);
    if (!res) return false;
  }

  if (repConf) {
    repConf->replicaPrivateKey =
        replicaConfig.getValue<std::string>("private_key");

    bool res = initializeSBFTCrypto(selfNumber, numOfReplicas, maxFaulty,
                                    maxSlow, config, replicaConfig,
                                    publicKeysOfReplicas, repConf);
    if (!res) return false;

    repConf->publicKeysOfReplicas = publicKeysOfReplicas;
    repConf->viewChangeTimerMillisec =
        config.getValue<uint16_t>("view_change_timeout");
    repConf->statusReportTimerMillisec =
        config.getValue<uint16_t>("status_time_interval");
    repConf->concurrencyLevel = config.getValue<uint16_t>("concurrency_level");

    repConf->replicaId = selfNumber;
    repConf->fVal = maxFaulty;
    repConf->cVal = maxSlow;
    repConf->numOfClientProxies = numOfPrincipals - numOfReplicas;

    // TODO(IG): add to config file
    repConf->autoViewChangeEnabled = true;

  } else {
    clConf->clientId = selfNumber;
    clConf->maxFaulty = maxFaulty;
    clConf->maxSlow = maxSlow;
  }

  return true;
}

}  // namespace concord
}  // namespace vmware
}  // namespace com

#endif  // CONCORD_CONFIG_PARSER_HPP
