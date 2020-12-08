// Copyright 2019-2020 VMware, all rights reserved

#include "OpenTracing.hpp"
#include "gtest/gtest.h"
#include "mocks.hpp"

#include "IStateTransfer.hpp"
#include "NullStateTransfer.hpp"
#include "config/configuration_manager.hpp"
#include "db_interfaces.h"
#include "direct_kv_db_adapter.h"
#include "endianness.hpp"
#include "memorydb/client.h"
#include "memorydb/key_comparator.h"
#include "status.hpp"
#include "storage/kvb_key_types.h"
#include "time/time_contract.hpp"
#include "time/time_reading.hpp"

#include "reconfiguration/pruning/kvb_pruning_sm.hpp"
#include "reconfiguration/pruning/pruning_exception.hpp"
#include "reconfiguration/pruning/pruning_serialization.hpp"
#include "reconfiguration/pruning/rsa_pruning_signer.hpp"
#include "reconfiguration/pruning/rsa_pruning_verifier.hpp"

#include <cryptopp/osrng.h>
#include <google/protobuf/timestamp.pb.h>
#include <google/protobuf/util/time_util.h>
#include "Logger.hpp"

#include <opentracing/tracer.h>

#include <cassert>
#include <cstdint>
#include <exception>
#include <memory>
#include <reconfiguration/pruning/rsa_pruning_verifier.hpp>
#include <set>
#include <string>
#include <utility>
#include <vector>

using namespace concord::config;
using namespace concord::kvbc;
using namespace concord::kvbc::v1DirectKeyValue;
using namespace concord::storage;
using namespace concord::storage::memorydb;
using namespace concord::time;
using namespace concordUtils;
using namespace concord::reconfiguration::pruning;
using google::protobuf::util::TimeUtil;

using concord::utils::openssl_crypto::AsymmetricPrivateKey;
using concord::utils::openssl_crypto::AsymmetricPublicKey;
using concord::utils::openssl_crypto::DeserializePublicKey;
using concord::utils::openssl_crypto::GenerateAsymmetricCryptoKeyPair;
using CryptoPP::AutoSeededRandomPool;
using std::pair;
using std::unique_ptr;

namespace {

const auto GENESIS_BLOCK_ID = BlockId{1};
const auto LAST_BLOCK_ID = BlockId{150};
const auto REPLICA_PRINCIPAL_ID_START = 0;
const auto CLIENT_PRINCIPAL_ID_START = 20000;

const auto now = ReadTime();

class TestStorage : public ILocalKeyValueStorageReadOnly,
                    public IBlocksAppender {
 public:
  Status get(const Key& key, Value& outValue) const override {
    BlockId outBlockId;
    return get(blockId_, key, outValue, outBlockId);
  }

  Status get(BlockId readVersion, const Sliver& key, Sliver& outValue,
             BlockId& outBlock) const override {
    outBlock = readVersion;
    return db_.get(keyGen_->dataKey(key, readVersion), outValue);
  }

  BlockId getLastBlock() const override { return blockId_; }

  Status getBlockData(BlockId, SetOfKeyValuePairs&) const override {
    EXPECT_TRUE(false) << "getBlockData() should not be called by this test";
    return Status::IllegalOperation(
        "getBlockData() not supported in test mode");
  }

  Status mayHaveConflictBetween(const Sliver&, BlockId, BlockId,
                                bool&) const override {
    EXPECT_TRUE(false)
        << "mayHaveConflictBetween() should not be called by this test";
    return Status::IllegalOperation(
        "mayHaveConflictBetween() not supported in test mode");
  }

  Status addBlock(const SetOfKeyValuePairs& updates, BlockId& outBlockId,
                  const concordUtils::SpanWrapper& p =
                      concordUtils::SpanWrapper{}) override {
    outBlockId = ++blockId_;
    for (const auto& u : updates) {
      const auto status =
          db_.put(keyGen_->dataKey(u.first, blockId_), u.second);
      if (!status.isOK()) {
        return status;
      }
    }
    return Status::OK();
  }

  void setLastBlockId(BlockId id) { blockId_ = id; }

  void setGenesisBlockId(BlockId id) { genesisBlockId_ = id; }

  BlockId getGenesisBlock() const override { return genesisBlockId_; }

 private:
  KeyComparator comp_{new DBKeyComparator{}};
  Client db_{comp_};
  BlockId genesisBlockId_{INITIAL_GENESIS_BLOCK_ID};
  BlockId blockId_{LAST_BLOCK_ID};
  std::unique_ptr<IDataKeyGenerator> keyGen_{
      std::make_unique<RocksKeyGenerator>()};
};

auto GetStrictBlocksDeleterMock() {
  return ::testing::StrictMock<MockBlockDeleter>{};
}

class TestStateTransfer : public bftEngine::impl::NullStateTransfer {
 public:
  void addOnTransferringCompleteCallback(
      std::function<void(uint64_t)> callback) override {
    callback_ = callback;
  }

  void complete() { callback_(0); }

 private:
  std::function<void(uint64_t)> callback_;
};

using ReplicaIDs = std::set<std::uint64_t>;

ReplicaIDs GetReplicaIDs(const ConcordConfiguration& config) {
  ReplicaIDs ret;

  for (auto node_id = 0u; node_id < config.scopeSize("node"); ++node_id) {
    const auto& node_config = config.subscope("node", node_id);
    const auto& replica_config = node_config.subscope("replica", 0);

    const auto ins_res =
        ret.insert(replica_config.getValue<std::uint64_t>("principal_id"));
    assert(ins_res.second);
  }

  return ret;
}

const ConcordConfiguration& NodeConfig(const ConcordConfiguration& config,
                                       std::size_t idx) {
  return config.subscope("node", idx);
}

auto test_span = opentracing::Tracer::Global() -> StartSpan("pruning_sm_test");

void CheckLatestPrunableResp(
    const concord::messages::LatestPrunableBlock& latest_prunable_resp,
    int replica_idx, const RSAPruningVerifier& verifier) {
  ASSERT_EQ(latest_prunable_resp.replica, replica_idx);
  ASSERT_TRUE(verifier.Verify(latest_prunable_resp));
}

void InitBlockchainStorage(TimeContract& tc, std::size_t replica_count,
                           TestStorage& s, bool offset_ts = false,
                           bool empty_blockchain = false,
                           bool increment_now = false) {
  for (auto i = 0u; i < replica_count; ++i) {
    // If increment_now=true, make it so that TimeContract::GetTime() returns a
    // big value, but the last block in storage has a normal timestamp of the
    // real now .
    const auto increment_duration = increment_now
                                        ? TimeUtil::HoursToDuration(24 * 356)
                                        : TimeUtil::NanosecondsToDuration(0);
    tc.Update("time_source_" + std::to_string(i), 0,
              now + increment_duration + TimeUtil::SecondsToDuration(i));
  }

  if (empty_blockchain) {
    s.setLastBlockId(0);
    ASSERT_EQ(s.getLastBlock(), 0);
    return;
  }

  // ensure that blockId_ == LAST_BLOCK_ID at the end
  s.setLastBlockId(GENESIS_BLOCK_ID - 1);
  const Sliver key{new char[1]{kKvbKeySummarizedTime}, 1};
  for (auto i = GENESIS_BLOCK_ID; i <= LAST_BLOCK_ID; ++i) {
    // Blocks are spaced in 1-second intervals, except for the last block which
    // uses the summarized time.
    auto ts = now - TimeUtil::SecondsToDuration(LAST_BLOCK_ID - i);
    if (offset_ts) {
      ts -= TimeUtil::MillisecondsToDuration(500);
    } else if (i == LAST_BLOCK_ID) {
      if (increment_now) {
        ts = now;
      } else {
        ts = tc.GetTime();
      }
    }

    const auto buf_size = ts.ByteSizeLong();
    Sliver buf(new char[buf_size], buf_size);
    ts.SerializeToArray(const_cast<char*>(buf.data()), buf_size);
    const SetOfKeyValuePairs block{{std::make_pair(key, buf)}};
    BlockId blockId;
    s.addBlock(block, blockId);

    // Make sure we've inserted the correct block ID.
    ASSERT_EQ(i, blockId);

    // Make sure our storage mock works properly.
    {
      BlockId outBlockId;
      Sliver out;
      const auto status = s.get(i, key, out, outBlockId);
      ASSERT_EQ(status, Status::OK());
      ASSERT_EQ(outBlockId, i);
      ASSERT_EQ(buf, out);
    }
  }

  // Make sure getLastBlock() works properly and it is equal to
  // LAST_BLOCK_ID.
  ASSERT_EQ(s.getLastBlock(), LAST_BLOCK_ID);
}

concord::messages::PruneRequest ConstructPruneRequest(
    const ConcordConfiguration& config, std::size_t client_idx,
    BlockId min_prunable_block_id = LAST_BLOCK_ID) {
  concord::messages::PruneRequest prune_req;
  const auto sender = config.subscope("node", client_idx)
                          .subscope("client_proxy", 0)
                          .getValue<std::uint64_t>("principal_id");
  prune_req.sender = sender;

  for (auto i = 0u; i < config.scopeSize("node"); ++i) {
    const auto& node_config = config.subscope("node", i);
    const auto& replica_config = node_config.subscope("replica", 0);

    auto& latest_block = prune_req.latest_prunable_block.emplace_back(
        concord::messages::LatestPrunableBlock());
    latest_block.replica =
        replica_config.getValue<std::uint64_t>("principal_id");
    // Send different block IDs.
    latest_block.block_id = min_prunable_block_id + i;

    const auto block_signer = RSAPruningSigner{GetNodeConfig(config, i)};
    block_signer.Sign(latest_block);
  }
  return prune_req;
}

// Tests follow.

TEST(pruning_sm_test, sign_verify_parse_configuration) {
  {
    ASSERT_ANY_THROW({
      const auto signer = RSAPruningSigner{EmptyConfiguration()};
    }) << "RSAPruningSigner cannot be constructed with no replicas in the "
          "configuraiton";
  }

  {
    ASSERT_THROW(
        { const auto verifier = RSAPruningVerifier{EmptyConfiguration()}; },
        PruningConfigurationException)
        << "RSAPruningVerifier cannot be constructed with no replicas in the "
           "configuraiton";
  }

  {
    const auto replicas = 0;
    const auto client_proxies = 4;
    const auto config = TestConfiguration(replicas, client_proxies);
    ASSERT_THROW({ const auto verifier = RSAPruningVerifier{config}; },
                 PruningConfigurationException)
        << "RSAPruningVerifier cannot be constructed with no replicas in the "
           "configuraiton";
  }

  {
    const auto replicas = 4;
    const auto client_proxies = 4;
    auto config = TestConfiguration(replicas, client_proxies);
    config.subscope("node", 0)
        .subscope("replica", 0)
        .loadValue("principal_id",
                   std::to_string(REPLICA_PRINCIPAL_ID_START + 1));
    ASSERT_THROW({ const auto verifier = RSAPruningVerifier{config}; },
                 PruningConfigurationException)
        << "RSAPruningVerifier cannot be constructed with duplicate replica "
           "principal_ids";
  }
}

TEST(pruning_sm_test, sm_parse_configuration) {
  TestStorage s;
  TestStateTransfer st;
  auto d = GetStrictBlocksDeleterMock();

  {
    const auto replicas = 4;
    const auto client_proxies = 4;
    ConcordConfiguration base_config =
        TestConfiguration(replicas, client_proxies);
    const auto& c = base_config;
    const auto& n = GetNodeConfig(c, 1);
    ASSERT_NO_THROW({
      TimeContract tc(s, c);
      KVBPruningSM sm(s, s, d, st, c, n, &tc);
    });
  }

  {
    const auto replicas = 4;
    const auto client_proxies = 8;
    ConcordConfiguration base_config =
        TestConfiguration(replicas, client_proxies);
    const auto& c = base_config;
    const auto& n = GetNodeConfig(c, 1);
    ASSERT_NO_THROW({
      TimeContract tc(s, c);
      KVBPruningSM sm(s, s, d, st, c, n, &tc);
    });
  }

  {
    const auto replicas = 4;
    const auto client_proxies = 2;
    ConcordConfiguration base_config =
        TestConfiguration(replicas, client_proxies);
    const auto& c = base_config;
    const auto& n = GetNodeConfig(c, 1);
    ASSERT_NO_THROW({
      TimeContract tc(s, c);
      KVBPruningSM sm(s, s, d, st, c, n, &tc);
    });
  }

  {
    const auto replicas = 7;
    const auto client_proxies = 3;
    ConcordConfiguration base_config =
        TestConfiguration(replicas, client_proxies);
    const auto& c = base_config;
    const auto& n = GetNodeConfig(c, 1);
    ASSERT_NO_THROW({
      TimeContract tc(s, c);
      KVBPruningSM sm(s, s, d, st, c, n, &tc);
    });
  }

  {
    const auto replicas = 4;
    const auto client_proxies = 0;
    ConcordConfiguration base_config =
        TestConfiguration(replicas, client_proxies);
    const auto& c = base_config;
    const auto& n = GetNodeConfig(c, 1);
    ASSERT_NO_THROW({
      TimeContract tc(s, c);
      KVBPruningSM sm(s, s, d, st, c, n, &tc);
    });
  }
}

TEST(pruning_sm_test, sign_verify_correct) {
  const auto replica_count = 4;
  const auto client_proxy_count = replica_count;
  ConcordConfiguration base_config =
      TestConfiguration(replica_count, client_proxy_count);

  const auto sending_id = 1;
  const auto& config = base_config;
  const auto verifier = RSAPruningVerifier{config};
  std::vector<RSAPruningSigner> signers;
  for (auto i = 0; i < replica_count; ++i) {
    signers.push_back(RSAPruningSigner{NodeConfig(config, i)});
  }

  // Sign and verify a LatestPrunableBlock message.
  {
    concord::messages::LatestPrunableBlock block;
    block.replica = REPLICA_PRINCIPAL_ID_START + sending_id;
    block.block_id = LAST_BLOCK_ID;
    signers[sending_id].Sign(block);

    ASSERT_TRUE(verifier.Verify(block));
  }

  // Sign and verify a PruneRequest message.
  {
    concord::messages::PruneRequest request;
    request.sender =
        CLIENT_PRINCIPAL_ID_START + client_proxy_count * sending_id;
    for (auto i = 0; i < replica_count; ++i) {
      auto& block = request.latest_prunable_block.emplace_back(
          concord::messages::LatestPrunableBlock());
      block.replica = REPLICA_PRINCIPAL_ID_START + i;
      block.block_id = LAST_BLOCK_ID;
      signers[i].Sign(block);
    }
    ASSERT_TRUE(verifier.Verify(request));
  }
}

TEST(pruning_sm_test, verify_malformed_messages) {
  const auto replica_count = 4;
  const auto client_proxy_count = replica_count;
  ConcordConfiguration base_config =
      TestConfiguration(replica_count, client_proxy_count);
  const auto sending_id = 1;
  const auto& config = base_config;
  const auto verifier = RSAPruningVerifier{config};
  std::vector<RSAPruningSigner> signers;
  for (auto i = 0; i < replica_count; ++i) {
    signers.push_back(RSAPruningSigner{NodeConfig(config, i)});
  }

  // Break verification of LatestPrunableBlock messages.
  {
    concord::messages::LatestPrunableBlock block;
    block.replica = REPLICA_PRINCIPAL_ID_START + sending_id;
    block.block_id = LAST_BLOCK_ID;
    signers[sending_id].Sign(block);

    // Change the replica ID after signing.
    block.replica = REPLICA_PRINCIPAL_ID_START + sending_id + 1;
    ASSERT_FALSE(verifier.Verify(block));

    // Make sure it works with the correct replica ID.
    block.replica = REPLICA_PRINCIPAL_ID_START + sending_id;
    ASSERT_TRUE(verifier.Verify(block));

    // Change the block ID after signing.
    block.block_id = LAST_BLOCK_ID + 1;
    ASSERT_FALSE(verifier.Verify(block));

    // Make sure it works with the correct block ID.
    block.block_id = LAST_BLOCK_ID;
    ASSERT_TRUE(verifier.Verify(block));

    // Change a single byte from the signature and make sure it doesn't verify.
    block.signature[0] += 1;
    ASSERT_FALSE(verifier.Verify(block));
  }

  // Change the sender in PruneRequest after signing and verify.
  {
    concord::messages::PruneRequest request;
    request.sender =
        CLIENT_PRINCIPAL_ID_START + client_proxy_count * sending_id;
    for (auto i = 0; i < replica_count; ++i) {
      auto& block = request.latest_prunable_block.emplace_back(
          concord::messages::LatestPrunableBlock());
      block.replica = REPLICA_PRINCIPAL_ID_START + i;
      block.block_id = LAST_BLOCK_ID;
      signers[i].Sign(block);
    }

    request.sender = request.sender + 1;

    ASSERT_TRUE(verifier.Verify(request));
  }

  // Verify a PruneRequest with replica_count - 1 latest prunable blocks.
  {
    concord::messages::PruneRequest request;
    request.sender =
        CLIENT_PRINCIPAL_ID_START + client_proxy_count * sending_id;
    for (auto i = 0; i < replica_count - 1; ++i) {
      auto& block = request.latest_prunable_block.emplace_back(
          concord::messages::LatestPrunableBlock());
      block.replica = REPLICA_PRINCIPAL_ID_START + i;
      block.block_id = LAST_BLOCK_ID;
      signers[i].Sign(block);
    }

    ASSERT_FALSE(verifier.Verify(request));
  }

  // Change replica in a single latest prunable block message after signing it
  {
    concord::messages::PruneRequest request;
    request.sender =
        CLIENT_PRINCIPAL_ID_START + client_proxy_count * sending_id;
    for (auto i = 0; i < replica_count; ++i) {
      auto& block = request.latest_prunable_block.emplace_back(
          concord::messages::LatestPrunableBlock());
      block.replica = REPLICA_PRINCIPAL_ID_START + i;
      block.block_id = LAST_BLOCK_ID;
      signers[i].Sign(block);
    }
    request.latest_prunable_block[0].replica =
        REPLICA_PRINCIPAL_ID_START + replica_count + 8;

    ASSERT_FALSE(verifier.Verify(request));
  }
}

TEST(pruning_sm_test, sm_latest_prunable_request_correct_num_bocks_to_keep) {
  const auto replica_count = 4;
  const auto client_proxy_count = replica_count;
  const auto num_blocks_to_keep = 30;
  const auto replica_idx = 1;
  ConcordConfiguration base_config =
      TestConfiguration(replica_count, client_proxy_count, num_blocks_to_keep);
  const auto& config = base_config;
  TestStorage storage;
  auto blocks_deleter = GetStrictBlocksDeleterMock();
  TestStateTransfer state_transfer;
  const auto verifier = RSAPruningVerifier{config};

  // Construct the pruning state machine with a nullptr TimeContract to verify
  // it works in case the time service is disabled.
  const auto sm =
      KVBPruningSM{storage,        storage, blocks_deleter,
                   state_transfer, config,  GetNodeConfig(config, replica_idx),
                   nullptr};

  concord::messages::LatestPrunableBlock resp;
  concord::messages::LatestPrunableBlockRequest req;
  sm.Handle(req, resp, *test_span);
  CheckLatestPrunableResp(resp, replica_idx, verifier);
  ASSERT_EQ(resp.block_id, LAST_BLOCK_ID - num_blocks_to_keep);
}

TEST(pruning_sm_test, sm_latest_prunable_request_big_num_blocks_to_keep) {
  const auto replica_count = 4;
  const auto client_proxy_count = replica_count;
  const auto num_blocks_to_keep = LAST_BLOCK_ID + 42;
  const auto replica_idx = 1;
  ConcordConfiguration base_config =
      TestConfiguration(replica_count, client_proxy_count, num_blocks_to_keep);
  const auto& config = base_config;
  TestStorage storage;
  auto blocks_deleter = GetStrictBlocksDeleterMock();
  TestStateTransfer state_transfer;
  const auto verifier = RSAPruningVerifier{config};

  auto tc = TimeContract{storage, config};
  const auto sm = KVBPruningSM{storage,
                               storage,
                               blocks_deleter,
                               state_transfer,
                               config,
                               GetNodeConfig(config, replica_idx),
                               &tc};

  concord::messages::LatestPrunableBlock resp;
  concord::messages::LatestPrunableBlockRequest req;
  sm.Handle(req, resp, *test_span);
  CheckLatestPrunableResp(resp, replica_idx, verifier);
  // Verify that the returned block ID is 0 when pruning_num_blocks_to_keep is
  // bigger than the latest block ID.
  ASSERT_EQ(resp.block_id, 0);
}

// The blockchain created in this test is represented by the following
// (id,timestamp[seconds]) pair list where LB=LAST_BLOCK_ID seconds and
// genesis block ID=1:
// -------------------------------------------------
// | 1;now-LB+1,2;now-LB+2,...,LB-1;now-1,LB;now+x |
// -------------------------------------------------
// The last block has a time (now+x) that is different from now, because of
// TimeContract's median-based summarized time handling (see
// InitBlockchainStorage()).
TEST(pruning_sm_test, sm_latest_prunable_request_time_range) {
  const auto replica_count = 7;
  const auto client_proxy_count = replica_count;
  const auto replica_idx = 1;
  const auto duration_to_keep_minutes = 1;
  ConcordConfiguration base_config = TestConfiguration(
      replica_count, client_proxy_count, 0, duration_to_keep_minutes);
  const auto& config = base_config;
  TestStorage storage;
  auto blocks_deleter = GetStrictBlocksDeleterMock();
  TestStateTransfer state_transfer;
  const auto verifier = RSAPruningVerifier{config};

  auto tc = TimeContract{storage, config};
  InitBlockchainStorage(tc, replica_count, storage);

  const auto sm = KVBPruningSM{storage,
                               storage,
                               blocks_deleter,
                               state_transfer,
                               config,
                               GetNodeConfig(config, replica_idx),
                               &tc};

  concord::messages::LatestPrunableBlock resp;
  concord::messages::LatestPrunableBlockRequest req;
  sm.Handle(req, resp, *test_span);
  CheckLatestPrunableResp(resp, replica_idx, verifier);
  const auto latest_prunable_ts =
      tc.GetTime() - TimeUtil::MinutesToDuration(duration_to_keep_minutes);
  const auto genesis_ts = tc.GetSummarizedTimeAtBlock(GENESIS_BLOCK_ID);
  const auto latest_prunable_block =
      TimeUtil::DurationToSeconds(latest_prunable_ts - genesis_ts) + 1;
  ASSERT_EQ(resp.block_id, latest_prunable_block);
}

// The blockchain created in this test is represented by the following
// (id,timestamp[seconds]) pair list where LB=LAST_BLOCK_ID seconds,
// genesis block ID=1 and time offset OFF=500ms :
// -------------------------------------------------------------
// | 1;now-LB+1-OFF,2;now-LB+2-OFF,...,LB-1;now-1-OFF,LB;now+x |
// -------------------------------------------------------------
// The intention of this test is to verify a case where the pruning state
// machine won't find an exact match on timestamp and will return the parent
// block of the one returned by std::lower_bound() .
//
// The last block has a time (now+x) that is different that now, because of
// TimeContract's median-based summarized time handling (see
// InitBlockchainStorage()).
TEST(pruning_sm_test, sm_latest_prunable_request_time_range_offset) {
  const auto replica_count = 7;
  const auto client_proxy_count = replica_count;
  const auto replica_idx = 1;
  const auto duration_to_keep_minutes = 1;
  ConcordConfiguration base_config = TestConfiguration(
      replica_count, client_proxy_count, 0, duration_to_keep_minutes);
  const auto& config = base_config;
  TestStorage storage;
  auto blocks_deleter = GetStrictBlocksDeleterMock();
  TestStateTransfer state_transfer;
  const auto verifier = RSAPruningVerifier{config};

  auto tc = TimeContract{storage, config};
  InitBlockchainStorage(tc, replica_count, storage, true);

  const auto sm = KVBPruningSM{storage,
                               storage,
                               blocks_deleter,
                               state_transfer,
                               config,
                               GetNodeConfig(config, replica_idx),
                               &tc};

  concord::messages::LatestPrunableBlock resp;
  concord::messages::LatestPrunableBlockRequest req;
  sm.Handle(req, resp, *test_span);
  CheckLatestPrunableResp(resp, replica_idx, verifier);
  const auto latest_prunable_ts =
      tc.GetTime() - TimeUtil::MinutesToDuration(duration_to_keep_minutes);
  const auto genesis_ts = tc.GetSummarizedTimeAtBlock(GENESIS_BLOCK_ID);
  const auto latest_prunable_block =
      TimeUtil::DurationToSeconds(latest_prunable_ts - genesis_ts) + 1;
  ASSERT_EQ(resp.block_id, latest_prunable_block);
}

// The blockchain created in this test is the same as the one in the
// sm_latest_prunable_request_time_range test.
TEST(pruning_sm_test, sm_latest_prunable_request_time_range_empty_chain) {
  const auto replica_count = 7;
  const auto client_proxy_count = replica_count;
  const auto replica_idx = 1;
  const auto duration_to_keep_minutes = 1;
  ConcordConfiguration base_config = TestConfiguration(
      replica_count, client_proxy_count, 0, duration_to_keep_minutes);
  const auto& config = base_config;
  TestStorage storage;
  auto blocks_deleter = GetStrictBlocksDeleterMock();
  TestStateTransfer state_transfer;
  const auto verifier = RSAPruningVerifier{config};

  auto tc = TimeContract{storage, config};
  InitBlockchainStorage(tc, replica_count, storage, false, true);

  const auto sm = KVBPruningSM{storage,
                               storage,
                               blocks_deleter,
                               state_transfer,
                               config,
                               GetNodeConfig(config, replica_idx),
                               &tc};

  concord::messages::LatestPrunableBlockRequest req;
  concord::messages::LatestPrunableBlock resp;

  sm.Handle(req, resp, *test_span);
  CheckLatestPrunableResp(resp, replica_idx, verifier);
  // Verify that 0 is returned when num_blocks_to_keep=0,
  // duration_to_keep_minutes=1 and there are no blocks in the blockchain.
  ASSERT_EQ(resp.block_id, 0);
}

// The blockchain created in this test is represented by the following
// (id,timestamp[seconds]) pair list where LB=LAST_BLOCK_ID seconds and
// genesis block ID=1:
// -----------------------------------------------
// | 1;now-LB+1,2;now-LB+2,...,LB-1;now-1,LB;now |
// -----------------------------------------------
// The intention of this test is to verify that if the last block has a
// timestamp that is significantly earlier than TimeContract::GetTime(), the
// pruning state machine will return the last block ID as the latest prunable
// one. This can happen if the TimeContract has been updated, but the current
// block hasn't been written to storage.
TEST(pruning_sm_test, sm_latest_prunable_request_time_range_missing_last) {
  const auto replica_count = 7;
  const auto client_proxy_count = replica_count;
  const auto replica_idx = 1;
  const auto duration_to_keep_minutes = 1;
  ConcordConfiguration base_config = TestConfiguration(
      replica_count, client_proxy_count, 0, duration_to_keep_minutes);
  const auto& config = base_config;
  TestStorage storage;
  auto blocks_deleter = GetStrictBlocksDeleterMock();
  TestStateTransfer state_transfer;
  const auto verifier = RSAPruningVerifier{config};

  auto tc = TimeContract{storage, config};
  InitBlockchainStorage(tc, replica_count, storage, false, false, true);

  const auto sm = KVBPruningSM{storage,
                               storage,
                               blocks_deleter,
                               state_transfer,
                               config,
                               GetNodeConfig(config, replica_idx),
                               &tc};

  concord::messages::LatestPrunableBlockRequest req;
  concord::messages::LatestPrunableBlock resp;

  sm.Handle(req, resp, *test_span);
  CheckLatestPrunableResp(resp, replica_idx, verifier);
  // Verify that the pruning state machine will return the last block ID as the
  // latest prunable one.
  ASSERT_EQ(resp.block_id, LAST_BLOCK_ID);
}

// The blockchain created in this test is the same as the one in the
// sm_latest_prunable_request_time_range test.
TEST(pruning_sm_test, sm_latest_prunable_request_time_range_and_num_blocks) {
  const auto replica_count = 7;
  const auto client_proxy_count = replica_count;
  const auto num_blocks_to_keep = LAST_BLOCK_ID - 5;  // prune a few blocks only
  const auto replica_idx = 1;
  const auto duration_to_keep_minutes = 1;
  ConcordConfiguration base_config =
      TestConfiguration(replica_count, client_proxy_count, num_blocks_to_keep,
                        duration_to_keep_minutes);
  const auto& config = base_config;
  TestStorage storage;
  auto blocks_deleter = GetStrictBlocksDeleterMock();
  TestStateTransfer state_transfer;
  const auto verifier = RSAPruningVerifier{config};

  auto tc = TimeContract{storage, config};
  InitBlockchainStorage(tc, replica_count, storage);

  const auto sm = KVBPruningSM{storage,
                               storage,
                               blocks_deleter,
                               state_transfer,
                               config,
                               GetNodeConfig(config, replica_idx),
                               &tc};

  concord::messages::LatestPrunableBlockRequest req;
  concord::messages::LatestPrunableBlock resp;

  sm.Handle(req, resp, *test_span);
  CheckLatestPrunableResp(resp, replica_idx, verifier);
  // Verify that the more conservative 'number of blocks to keep' option takes
  // precedence.
  ASSERT_EQ(resp.block_id, LAST_BLOCK_ID - num_blocks_to_keep);
}

// The blockchain created in this test is the same as the one in the
// sm_latest_prunable_request_time_range test.
TEST(pruning_sm_test, sm_latest_prunable_request_no_pruning_conf) {
  const auto replica_count = 7;
  const auto client_proxy_count = replica_count;
  const auto num_blocks_to_keep = 0;
  const auto replica_idx = 1;
  const auto duration_to_keep_minutes = 0;
  ConcordConfiguration base_config =
      TestConfiguration(replica_count, client_proxy_count, num_blocks_to_keep,
                        duration_to_keep_minutes);
  const auto& config = base_config;
  TestStorage storage;
  auto blocks_deleter = GetStrictBlocksDeleterMock();
  TestStateTransfer state_transfer;
  const auto verifier = RSAPruningVerifier{config};

  auto tc = TimeContract{storage, config};
  InitBlockchainStorage(tc, replica_count, storage);

  const auto sm = KVBPruningSM{storage,
                               storage,
                               blocks_deleter,
                               state_transfer,
                               config,
                               GetNodeConfig(config, replica_idx),
                               &tc};

  concord::messages::LatestPrunableBlockRequest req;
  concord::messages::LatestPrunableBlock resp;

  sm.Handle(req, resp, *test_span);
  CheckLatestPrunableResp(resp, replica_idx, verifier);
  // Verify that when pruning is enabled and both pruning_num_blocks_to_keep and
  // duration_to_keep_minutes are set to 0, then LAST_BLOCK_ID will be returned.
  ASSERT_EQ(resp.block_id, LAST_BLOCK_ID);
}

TEST(pruning_sm_test, sm_latest_prunable_request_pruning_disabled) {
  const auto replica_count = 7;
  const auto client_proxy_count = replica_count;
  const auto num_blocks_to_keep = 0;
  const auto replica_idx = 1;
  const auto duration_to_keep_minutes = 0;
  const auto config =
      TestConfiguration(replica_count, client_proxy_count, num_blocks_to_keep,
                        duration_to_keep_minutes, false);
  TestStorage storage;
  auto blocks_deleter = GetStrictBlocksDeleterMock();
  TestStateTransfer state_transfer;
  const auto verifier = RSAPruningVerifier{config};

  auto tc = TimeContract{storage, config};
  InitBlockchainStorage(tc, replica_count, storage);

  const auto sm = KVBPruningSM{storage,
                               storage,
                               blocks_deleter,
                               state_transfer,
                               config,
                               GetNodeConfig(config, replica_idx),
                               &tc};

  concord::messages::LatestPrunableBlockRequest req;
  concord::messages::LatestPrunableBlock resp;
  sm.Handle(req, resp, *test_span);

  CheckLatestPrunableResp(resp, replica_idx, verifier);
  // Verify that when pruning is disabled, 0 is returned.
  ASSERT_EQ(resp.block_id, 0);
}

TEST(pruning_sm_test, sm_handle_prune_request_on_pruning_disabled) {
  const auto replica_count = 4;
  const auto client_proxy_count = replica_count;
  const auto num_blocks_to_keep = 30;
  const auto replica_idx = 1;
  const auto duration_to_keep_minutes = 0;
  const auto client_idx = 0;
  ConcordConfiguration base_config =
      TestConfiguration(replica_count, client_proxy_count, num_blocks_to_keep,
                        duration_to_keep_minutes, false);
  const auto& config = base_config;
  TestStorage storage;
  auto blocks_deleter = GetStrictBlocksDeleterMock();
  TestStateTransfer state_transfer;

  auto tc = TimeContract{storage, config};
  const auto sm = KVBPruningSM{storage,
                               storage,
                               blocks_deleter,
                               state_transfer,
                               config,
                               GetNodeConfig(config, replica_idx),
                               &tc};

  const auto req = ConstructPruneRequest(config, client_idx);
  uint64_t agreed_pruned_block;
  auto res = sm.Handle(req, false, *test_span).has_value();
  ASSERT_FALSE(res);
}

TEST(pruning_sm_test, sm_handle_correct_prune_request) {
  const auto replica_count = 4;
  const auto client_proxy_count = replica_count;
  const auto num_blocks_to_keep = 30;
  const auto replica_idx = 1;
  const auto client_idx = 0;
  ConcordConfiguration base_config =
      TestConfiguration(replica_count, client_proxy_count, num_blocks_to_keep);
  const auto& config = base_config;
  TestStorage storage;
  auto blocks_deleter = GetStrictBlocksDeleterMock();
  TestStateTransfer state_transfer;

  auto tc = TimeContract{storage, config};
  const auto sm = KVBPruningSM{storage,
                               storage,
                               blocks_deleter,
                               state_transfer,
                               config,
                               GetNodeConfig(config, replica_idx),
                               &tc};

  const auto latest_prunable_block_id =
      storage.getLastBlock() - num_blocks_to_keep;
  const auto req =
      ConstructPruneRequest(config, client_idx, latest_prunable_block_id);

  // Add 1 as deleteBlocksUntil() deletes in the [genesis, to) range.
  EXPECT_CALL(blocks_deleter, deleteBlocksUntil(latest_prunable_block_id + 1))
      .Times(1);
  uint64_t agreed_pruned_block;
  auto res = sm.Handle(req, false, *test_span).has_value();

  ASSERT_TRUE(res);

  // Make sure the state machine has added the last agreed prunable block ID
  // key.
  auto key = Value{};
  ASSERT_TRUE(
      storage.get(KVBPruningSM::LastAgreedPrunableBlockIdKey(), key).isOK());
  ASSERT_EQ(key.length(), sizeof(BlockId));
  const auto persisted_last_agreed =
      concordUtils::fromBigEndianBuffer<BlockId>(key.data());
  ASSERT_EQ(latest_prunable_block_id, persisted_last_agreed);
}

TEST(pruning_sm_test, sm_prune_on_startup) {
  const auto replica_count = 4;
  const auto client_proxy_count = replica_count;
  const auto num_blocks_to_keep = 30;
  const auto replica_idx = 1;
  ConcordConfiguration base_config =
      TestConfiguration(replica_count, client_proxy_count, num_blocks_to_keep);
  const auto& config = base_config;
  TestStorage storage;
  auto blocks_deleter = GetStrictBlocksDeleterMock();
  TestStateTransfer state_transfer;

  const auto last_agreed_prunable_block_id =
      BlockId{storage.getLastBlock() - num_blocks_to_keep};

  // Simulate a situation in which the replica hasn't fully executed pruning on
  // startup. For example, that might happen due to a crash just before pruning
  // has started, but after the last agreed prunable block ID has been
  // persisted.
  auto added_block_id = BlockId{0};
  storage.addBlock(SetOfKeyValuePairs{std::make_pair(
                       KVBPruningSM::LastAgreedPrunableBlockIdKey(),
                       Sliver{concordUtils::toBigEndianStringBuffer(
                           last_agreed_prunable_block_id)})},
                   added_block_id);

  // Add 1 as deleteBlocksUntil() deletes in the [genesis, to) range.
  // Expect to be called on construction of the pruning state machine.
  EXPECT_CALL(blocks_deleter,
              deleteBlocksUntil(last_agreed_prunable_block_id + 1))
      .Times(1);

  auto tc = TimeContract{storage, config};
  const auto sm = KVBPruningSM{storage,
                               storage,
                               blocks_deleter,
                               state_transfer,
                               config,
                               GetNodeConfig(config, replica_idx),
                               &tc};
}

TEST(pruning_sm_test, sm_already_pruned_on_startup) {
  const auto replica_count = 4;
  const auto client_proxy_count = replica_count;
  const auto num_blocks_to_keep = 30;
  const auto replica_idx = 1;
  ConcordConfiguration base_config =
      TestConfiguration(replica_count, client_proxy_count, num_blocks_to_keep);
  const auto& config = base_config;
  TestStorage storage;
  auto blocks_deleter = GetStrictBlocksDeleterMock();
  TestStateTransfer state_transfer;

  const auto last_agreed_prunable_block_id =
      BlockId{storage.getLastBlock() - num_blocks_to_keep};

  auto added_block_id = BlockId{0};
  storage.addBlock(SetOfKeyValuePairs{std::make_pair(
                       KVBPruningSM::LastAgreedPrunableBlockIdKey(),
                       Sliver{concordUtils::toBigEndianStringBuffer(
                           last_agreed_prunable_block_id)})},
                   added_block_id);

  // Make sure the genesis block ID is bigger than the agreed prunable block ID.
  // In that case, the state machine should not call any methods on
  // 'blocks_deleter' (ensured by the strict mock).
  storage.setGenesisBlockId(last_agreed_prunable_block_id + 1);

  auto tc = TimeContract{storage, config};
  const auto sm = KVBPruningSM{storage,
                               storage,
                               blocks_deleter,
                               state_transfer,
                               config,
                               GetNodeConfig(config, replica_idx),
                               &tc};
}

TEST(pruning_sm_test, sm_prune_on_state_transfer_complete) {
  const auto replica_count = 4;
  const auto client_proxy_count = replica_count;
  const auto num_blocks_to_keep = 30;
  const auto replica_idx = 1;
  ConcordConfiguration base_config =
      TestConfiguration(replica_count, client_proxy_count, num_blocks_to_keep);
  const auto& config = base_config;
  TestStorage storage;
  auto blocks_deleter = GetStrictBlocksDeleterMock();
  TestStateTransfer state_transfer;

  auto tc = TimeContract{storage, config};
  const auto sm = KVBPruningSM{storage,
                               storage,
                               blocks_deleter,
                               state_transfer,
                               config,
                               GetNodeConfig(config, replica_idx),
                               &tc};

  const auto last_agreed_prunable_block_id =
      BlockId{storage.getLastBlock() - num_blocks_to_keep};

  // Simulate a situation in which the current replica has missed blocks,
  // pruning has been executed on other replicas and state transfer completes,
  // bringing in a block that contains the last agreed prunable block ID.
  auto added_block_id = BlockId{0};
  storage.addBlock(SetOfKeyValuePairs{std::make_pair(
                       KVBPruningSM::LastAgreedPrunableBlockIdKey(),
                       Sliver{concordUtils::toBigEndianStringBuffer(
                           last_agreed_prunable_block_id)})},
                   added_block_id);

  // Add 1 as deleteBlocksUntil() deletes in the [genesis, to) range.
  // Expect to be called when state transfer has completed.
  EXPECT_CALL(blocks_deleter,
              deleteBlocksUntil(last_agreed_prunable_block_id + 1));
  state_transfer.complete();
}

TEST(pruning_sm_test, sm_handle_incorrect_prune_request) {
  const auto replica_count = 4;
  const auto client_proxy_count = replica_count;
  const auto num_blocks_to_keep = 30;
  const auto replica_idx = 1;
  const auto client_idx = 0;
  ConcordConfiguration base_config =
      TestConfiguration(replica_count, client_proxy_count, num_blocks_to_keep);
  const auto& config = base_config;
  TestStorage storage;
  auto blocks_deleter = GetStrictBlocksDeleterMock();
  TestStateTransfer state_transfer;

  auto tc = TimeContract{storage, config};
  const auto sm = KVBPruningSM{storage,
                               storage,
                               blocks_deleter,
                               state_transfer,
                               config,
                               GetNodeConfig(config, replica_idx),
                               &tc};

  // Add a valid N + 1 latest prunable block.
  {
    auto req = ConstructPruneRequest(config, client_idx);
    const auto& block = req.latest_prunable_block[3];
    auto& added_block = req.latest_prunable_block.emplace_back(
        concord::messages::LatestPrunableBlock());
    added_block.block_id = block.block_id;
    added_block.replica = block.replica;
    added_block.signature = block.signature;
    uint64_t agreed_pruned_block;
    auto res = sm.Handle(req, false, *test_span).has_value();

    // Expect that the state machine has ignored the message.
    ASSERT_FALSE(res);
  }

  // Send N - 1 latest prunable blocks.
  {
    auto req = ConstructPruneRequest(config, client_idx);
    req.latest_prunable_block.pop_back();
    uint64_t agreed_pruned_block;
    auto res = sm.Handle(req, false, *test_span).has_value();

    // Expect that the state machine has ignored the message.
    ASSERT_FALSE(res);
  }

  // Send a latest prunable block with an invalid signature.
  {
    auto req = ConstructPruneRequest(config, client_idx);
    auto& block =
        req.latest_prunable_block[req.latest_prunable_block.size() - 1];
    block.signature[0] += 1;
    uint64_t agreed_pruned_block;
    auto res = sm.Handle(req, false, *test_span).has_value();

    // Expect that the state machine has ignored the message.
    ASSERT_FALSE(res);
  }

  // Send a valid prune request in a read-only message.
  {
    const auto req = ConstructPruneRequest(config, client_idx);
    uint64_t agreed_pruned_block;
    auto res = sm.Handle(req, true, *test_span).has_value();

    // Expect that the state machine has ignored the message.
    ASSERT_FALSE(res);
  }
}

}  // anonymous namespace

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
