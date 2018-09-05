// Copyright 2018 VMware, all rights reserved
//
// Temporary solution for creating configuration structs for concord-bft
// Ideally, the ReplicaConfig from BlockchainInterfaces.h

#ifndef ATHENA_CONFIG_PARSER_HPP
#define ATHENA_CONFIG_PARSER_HPP

#include "BlockchainInterfaces.h"
#include "../../submodules/concord-bft/threshsign/include/threshsign/IThresholdSigner.h"
#include "../../submodules/concord-bft/threshsign/include/threshsign/IThresholdVerifier.h"
#include "../../submodules/concord-bft/threshsign/include/threshsign/IThresholdFactory.h"
#include "../../submodules/concord-bft/bftengine/include/communication/CommFactory.hpp"
#include "../../submodules/concord-bft/bftengine/include/communication/CommImpl.hpp"
#include <string>
#include <set>

namespace com {
namespace vmware {
namespace athena {

const size_t MAX_ITEM_LENGTH = 4096;
const std::string MAX_ITEM_LENGTH_STR = std::to_string(MAX_ITEM_LENGTH);

inline void
read_one_line (FILE *f, int capacity, char * buf) {
   // Read next line
   fgets(buf, capacity - 1, f);
   buf[strlen(buf) - 1] = 0; // strip newline
}

inline void
ignore_one_line (FILE *f) {
   char buf[8192];
   read_one_line(f, 8191, buf);
}

inline IThresholdFactory*
create_threshold_factory(FILE* config_file)
{
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

   if(strcmp(cryptosys, MULTISIG_BLS_SCHEME) == 0) {
      return new BlsThresholdFactory(PublicParametersFactory::
                                        getByCurveType(curveType));
   } else if(strcmp(cryptosys, THRESHOLD_BLS_SCHEME) == 0) {
      return new BlsThresholdFactory(PublicParametersFactory::
                                        getByCurveType(curveType));
   } else {
      return nullptr;
   }
}

inline IThresholdSigner *
createThresholdSigner(FILE *config_priv, int id, IThresholdFactory * factory) {
   if(id < 1) {
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
createThresholdVerifier(FILE *config_file,
                        int requestedSigners,
                        int totalSigners,
                        IThresholdFactory* factory) {
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
   for(int i = 0; i < totalSigners; i++)
   {
      read_one_line(config_file, 4096, verifKey);
      //cout << "Read share verif key '" << verifKey << "'" << endl;
      verifKeys.push_back(std::string(verifKey));
   }

   // Ignore last comment/empty line
   ignore_one_line(config_file);

   return factory->newVerifier( requestedSigners,
                                totalSigners,
                                publicKey,
                                verifKeys);
}

inline bool
parse_crypto (uint16_t replicaId,
              uint16_t numOfReplicas,
              uint16_t maxFaulty,
              uint16_t maxSlow,
              FILE *publicKeysFile,
              FILE *privateKeysFile,
              Blockchain::ReplicaConsensusConfig *outConfig)
{
   int tempInt = 0;
   char tempString[MAX_ITEM_LENGTH];

   std::set <pair<uint16_t, string>> publicKeysOfReplicas;

   // read from publicKeysFile

   for (int i = 0; i < numOfReplicas; i++)
   {
      fscanf( publicKeysFile,
              string("%" + MAX_ITEM_LENGTH_STR + "s\n").c_str(),
              tempString);

      string name(tempString);

      string expectedName = string("replica") + std::to_string(i);

      if (expectedName != name) return false;

      fscanf( publicKeysFile,
              string("%" + MAX_ITEM_LENGTH_STR + "s\n").c_str(),
              tempString);

      string publicKeyString(tempString);

      publicKeysOfReplicas.insert({i, publicKeyString});
   }

   // Read threshold signatures
   IThresholdFactory* slowCommitFactory = nullptr;
   IThresholdSigner* thresholdSignerForSlowPathCommit = nullptr;
   IThresholdVerifier* thresholdVerifierForSlowPathCommit = nullptr;

   IThresholdFactory* optimisticCommitFactory = nullptr;
   IThresholdSigner* thresholdSignerForOptimisticCommit = nullptr;
   IThresholdVerifier* thresholdVerifierForOptimisticCommit = nullptr;

   slowCommitFactory = create_threshold_factory(publicKeysFile);
   thresholdVerifierForSlowPathCommit =
           createThresholdVerifier(publicKeysFile,
                                   numOfReplicas - maxFaulty,
                                   numOfReplicas,
                                   slowCommitFactory);

   optimisticCommitFactory = create_threshold_factory(publicKeysFile);
   thresholdVerifierForOptimisticCommit =
           createThresholdVerifier(publicKeysFile,
                                   numOfReplicas,
                                   numOfReplicas,
                                   optimisticCommitFactory);

   // read from privateKeysFile

   fscanf(privateKeysFile, "%d\n", &tempInt);
   if (tempInt != replicaId) return false;

   fscanf(privateKeysFile,
          string("%" + MAX_ITEM_LENGTH_STR + "s\n").c_str(),
          tempString);

   string sigPrivateKey(tempString);

   thresholdSignerForSlowPathCommit =
           createThresholdSigner( privateKeysFile,
                                  replicaId+1,
                                  slowCommitFactory);

   thresholdSignerForOptimisticCommit =
           createThresholdSigner( privateKeysFile,
                                  replicaId+1,
                                  optimisticCommitFactory);

   delete slowCommitFactory;
   delete optimisticCommitFactory;

   outConfig->publicKeysOfReplicas = publicKeysOfReplicas;
   outConfig->replicaPrivateKey = sigPrivateKey;

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
parse_comm_params(FILE *config_file,
                  uint16_t selfNumber,
                  uint16_t numOfPrincipals,
                  bftEngine::BaseCommConfig *outCommConfig) {


   // read in all the principals
   char addr_buff[100];
   char host_name[100];// not in use
   uint16_t port;
   char pk[4096];
   for(int i = 0; i < numOfPrincipals; i++) {
      fscanf( config_file,
              "%256s %32s %hd\n%4096s\n",
              host_name,
              addr_buff,
              &port,
              pk);
      if(outCommConfig) {
         outCommConfig->nodes.insert({i,
                                      std::make_tuple(string(addr_buff),
                                                      port)});
         if (i == selfNumber) {
            outCommConfig->listenPort = port;
         }
      }
   }

   if(outCommConfig) {
      /// TODO(IG): add to config file
      outCommConfig.bufferLength = 64000;
      outCommConfig.listenIp = "0.0.0.0";
   }
}

inline bool
parse_plain_config_file(std::string privPath,
                        std::string pubPath,
                        bftEngine::BaseCommConfig *commConfig = nullptr,
                        Blockchain::ClientConsensusConfig *clConf = nullptr,
                        Blockchain::ReplicaConsensusConfig *repConf = nullptr) {
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
   fscanf(config_file, "%*[^\n]\n", NULL);

   // read max_faulty
   uint16_t max_faulty = 0;
   fscanf(config_file, "%hu\n", &max_faulty);

   // read max_slow
   uint16_t max_slow = 0;
   fscanf(config_file, "%hu\n", &max_slow);

   //skip timeout
   fscanf(config_file, "%*[^\n]\n", NULL);

   // read num of principals
   uint16_t numOfPrincipals = 0;
   fscanf(config_file, "%hu\n", &numOfPrincipals);

   if(commConfig) {
      auto res = parse_comm_params(config_file,
                                   selfNumber,
                                   numOfPrincipals,
                                   commConfig);
      if (!res)
         return false;
   }

   if(repConf) {
      uint16_t numOfReplicas = 3 * max_faulty + 2 * max_slow + 1;
      auto res = parse_crypto(
              selfNumber,
              numOfReplicas,
              max_faulty,
              max_slow,
              config_file,
              config_priv_file,
              repConf);
      if(!res)
         return false;

      uint16_t viewChangeTimeoutMilli, statusTimeMilli, concurrencyLevel;
      fscanf(config_file, "%hu\n", &viewChangeTimeoutMilli);
      fscanf(config_file, "%hu\n", &statusTimeMilli);
      //skip one line
      fscanf(config_file, "%*[^\n]\n", NULL);
      fscanf(config_file, "%hu\n", &concurrencyLevel);

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
