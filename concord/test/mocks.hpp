// Copyright 2020 VMware, all rights reserved

#pragma once

// Keep googletest includes on top as the Assert macro from assertUtils.hpp can
// interfere.
#include "gmock/gmock.h"
#include "gtest/gtest.h"

#include "time/time_contract.hpp"

#include <concord.pb.h>
#include <daml_commit.pb.h>
#include <log4cplus/configurator.h>
#include <log4cplus/hierarchy.h>
#include <daml/daml_kvb_commands_handler.hpp>
#include <daml/daml_validator_client.hpp>
#include <utils/concord_prometheus_metrics.hpp>
#include "db_interfaces.h"

using namespace concord::kvbc;
using namespace concord::daml;
using namespace concord::utils;
using namespace concordUtils;

namespace da_kvbc = com::digitalasset::kvbc;

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::DamlRequest;
using concord::config::ConcordConfiguration;
using concord::config::ConfigurationPath;

using concord::thin_replica::SubBufferList;

using CryptoPP::AutoSeededRandomPool;

class MockDamlValidatorClient : public IDamlValidatorClient {
 public:
  MOCK_METHOD7(ValidateSubmission,
               grpc::Status(std::string, std::string,
                            google::protobuf::Timestamp&, std::string,
                            std::string, opentracing::Span&,
                            da_kvbc::ValidateResponse*));

  MOCK_METHOD5(ValidatePendingSubmission,
               grpc::Status(std::string,
                            const std::map<std::string, std::string>&,
                            std::string, opentracing::Span&,
                            da_kvbc::ValidatePendingSubmissionResponse*));
};

class MockLocalKeyValueStorageReadOnly : public ILocalKeyValueStorageReadOnly {
 public:
  MOCK_CONST_METHOD2(get, Status(const Key&, Value&));
  MOCK_CONST_METHOD4(get, Status(BlockId, const Sliver&, Sliver&, BlockId&));
  MOCK_CONST_METHOD0(getLastBlock, BlockId());
  MOCK_CONST_METHOD2(getBlockData, Status(BlockId, SetOfKeyValuePairs&));
  MOCK_CONST_METHOD4(mayHaveConflictBetween,
                     Status(const Sliver&, BlockId, BlockId, bool&));
  MOCK_CONST_METHOD0(monitor, void());
};

class MockBlockAppender : public IBlocksAppender {
 public:
  MOCK_METHOD2(addBlock, Status(const SetOfKeyValuePairs&, BlockId&));
};

class MockPrometheusRegistry : public IPrometheusRegistry {
 public:
  MOCK_METHOD1(scrapeRegistry, void(std::shared_ptr<prometheus::Collectable>));
  MOCK_METHOD3(createCounterFamily,
               prometheus::Family<prometheus::Counter>&(
                   const std::string&, const std::string&,
                   const std::map<std::string, std::string>&));
  MOCK_METHOD2(createCounter,
               prometheus::Counter&(prometheus::Family<prometheus::Counter>&,
                                    const std::map<std::string, std::string>&));
  MOCK_METHOD3(createCounter,
               prometheus::Counter&(const std::string&, const std::string&,
                                    const std::map<std::string, std::string>&));
  MOCK_METHOD3(createGaugeFamily,
               prometheus::Family<prometheus::Gauge>&(
                   const std::string&, const std::string&,
                   const std::map<std::string, std::string>&));
  MOCK_METHOD2(createGauge,
               prometheus::Gauge&(prometheus::Family<prometheus::Gauge>&,
                                  const std::map<std::string, std::string>&));
  MOCK_METHOD3(createGauge,
               prometheus::Gauge&(const std::string&, const std::string&,
                                  const std::map<std::string, std::string>&));
};

class MockTimeContract : public concord::time::TimeContract {
 public:
  MockTimeContract(const concord::kvbc::ILocalKeyValueStorageReadOnly& storage,
                   const concord::config::ConcordConfiguration& config,
                   const google::protobuf::Timestamp& time)
      : TimeContract(storage, config), time_(time){};

  google::protobuf::Timestamp GetTime() override { return time_; };

 private:
  google::protobuf::Timestamp time_;
};

ConcordConfiguration::ParameterStatus NodeScopeSizer(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    size_t* output, void* state);

ConcordConfiguration::ParameterStatus ReplicaScopeSizer(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    size_t* output, void* state);

ConcordConfiguration::ParameterStatus ClientProxyScopeSizer(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    size_t* output, void* state);

ConcordConfiguration TestConfiguration(
    std::size_t replica_count, std::size_t proxies_per_replica,
    std::uint64_t num_blocks_to_keep = 0,
    std::uint32_t duration_to_keep_minutes = 0, bool pruning_enabled = true,
    bool time_service_enabled = true);

const ConcordConfiguration& GetNodeConfig(const ConcordConfiguration& config,
                                          int index);

ConcordConfiguration EmptyConfiguration();