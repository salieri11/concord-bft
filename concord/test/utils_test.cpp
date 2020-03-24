/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

#include <nlohmann/json.hpp>

#include "gtest/gtest.h"
#include "utils/concord_prometheus_metrics.hpp"
#include "utils/concord_utils.hpp"
#include "utils/rlp.hpp"

using namespace std;
using json = nlohmann::json;
using boost::multiprecision::uint256_t;

using concord::utils::from_evm_uint256be;
using concord::utils::from_uint256_t;
using concord::utils::RLPBuilder;
using concord::utils::to_evm_uint256be;
using concord::utils::to_uint256_t;

namespace {
TEST(utils_test, parses_genesis_block) {
  // string genesis_test_file = "resources/genesis.json";
  // json pj = parse_genesis_block(genesis_test_file);
  // int chainID = pj["config"]["chainId"];
  // ASSERT_EQ(chainID, 1);
}

// examples from https://github.com/ethereum/wiki/wiki/RLP
TEST(rlp_test, example_dog) {
  RLPBuilder rlpb;
  std::vector<uint8_t> input{'d', 'o', 'g'};
  rlpb.add(input);
  std::vector<uint8_t> expect{0x83, 'd', 'o', 'g'};
  EXPECT_EQ(expect, rlpb.build());
}

TEST(rlp_test, example_cat_dog) {
  RLPBuilder rlpb;
  rlpb.start_list();
  std::vector<uint8_t> input1{'d', 'o', 'g'};
  rlpb.add(input1);
  std::vector<uint8_t> input2{'c', 'a', 't'};
  rlpb.add(input2);
  std::vector<uint8_t> expect{0xc8, 0x83, 'c', 'a', 't', 0x83, 'd', 'o', 'g'};
  EXPECT_EQ(expect, rlpb.build());
}

TEST(rlp_test, example_empty_string) {
  RLPBuilder rlpb;
  std::vector<uint8_t> input;
  rlpb.add(input);
  std::vector<uint8_t> expect{0x80};
  EXPECT_EQ(expect, rlpb.build());
}

TEST(rlp_test, example_empty_list) {
  RLPBuilder rlpb;
  rlpb.start_list();
  std::vector<uint8_t> expect{0xc0};
  EXPECT_EQ(expect, rlpb.build());
}

TEST(rlp_test, example_integer_0) {
  RLPBuilder rlpb;
  rlpb.add(0);
  std::vector<uint8_t> expect{0x80};
  EXPECT_EQ(expect, rlpb.build());
}

TEST(rlp_test, example_integer_15) {
  RLPBuilder rlpb;
  rlpb.add(15);
  std::vector<uint8_t> expect{0x0f};
  EXPECT_EQ(expect, rlpb.build());
}

TEST(rlp_test, example_integer_1024) {
  RLPBuilder rlpb;
  rlpb.add(1024);
  std::vector<uint8_t> expect{0x82, 0x04, 0x00};
  EXPECT_EQ(expect, rlpb.build());
}

TEST(rlp_test, example_nexted_list) {
  RLPBuilder rlpb;

  // remember that RLPBuilder expects additions in reverse order, so read the
  // test case backward to understand the code: [ [], [[]], [ [], [[]] ] ]
  rlpb.start_list();
  {
    rlpb.start_list();
    {
      rlpb.start_list();
      {
        rlpb.start_list();
        rlpb.end_list();
      }
      rlpb.end_list();

      rlpb.start_list();
      rlpb.end_list();
    }
    rlpb.end_list();

    rlpb.start_list();
    {
      rlpb.start_list();
      rlpb.end_list();
    }
    rlpb.end_list();

    rlpb.start_list();
    rlpb.end_list();
  }  // implicit end
  std::vector<uint8_t> expect{0xc7, 0xc0, 0xc1, 0xc0, 0xc3, 0xc0, 0xc1, 0xc0};
  EXPECT_EQ(expect, rlpb.build());
}

TEST(rlp_test, example_lipsum) {
  RLPBuilder rlpb;
  std::string str("Lorem ipsum dolor sit amet, consectetur adipisicing elit");
  std::vector<uint8_t> input(str.begin(), str.end());
  rlpb.add(input);
  std::vector<uint8_t> expect{0xb8, 0x38};
  std::copy(input.begin(), input.end(), std::back_inserter(expect));
  EXPECT_EQ(expect, rlpb.build());
}

TEST(utils_test, to_evm_uint256be_test) {
  uint64_t val = 0xabcd1234;
  evm_uint256be expected;
  to_evm_uint256be(val, &expected);
  EXPECT_EQ(expected.bytes[31], 0x34);
  EXPECT_EQ(expected.bytes[30], 0x12);
  EXPECT_EQ(expected.bytes[29], 0xcd);
  EXPECT_EQ(expected.bytes[28], 0xab);
  for (int i = 0; i < 28; i++) {
    EXPECT_EQ(expected.bytes[i], 0x00);
  }
}

TEST(utils_test, from_evm_uint256be_test) {
  uint64_t expected = 0x12121212abcd1234;
  evm_uint256be val;
  for (int i = 0; i < 28; i++) {
    val.bytes[i] = 0x12;
  }
  val.bytes[28] = 0xab;
  val.bytes[29] = 0xcd;
  val.bytes[30] = 0x12;
  val.bytes[31] = 0x34;
  EXPECT_EQ(expected, from_evm_uint256be(&val));
}

TEST(utils_test, to_uint256_t_test) {
  evm_uint256be input{0};
  input.bytes[31] = 0xef;
  input.bytes[30] = 0xbe;
  input.bytes[29] = 0xad;
  input.bytes[28] = 0xde;

  uint256_t expected{"0xdeadbeef"};
  EXPECT_EQ(expected, to_uint256_t(&input));
}

TEST(utils_test, from_uint256_t_test) {
  uint256_t input{"0xdeadbeef"};

  evm_uint256be expected{0};
  expected.bytes[31] = 0xef;
  expected.bytes[30] = 0xbe;
  expected.bytes[29] = 0xad;
  expected.bytes[28] = 0xde;
  evm_uint256be out = from_uint256_t(&input);
  EXPECT_EQ(0, memcmp(expected.bytes, out.bytes, sizeof(evm_uint256be)));
}

TEST(utils_test, test_configuration_parser) {
  auto conf = concord::utils::PrometheusRegistry::parseConfiguration(
      "resources/metrics_config.yaml");
  ASSERT_EQ(conf[0].name_, "concord_command_handler_operation_counters_total");
  ASSERT_EQ(conf[0].type_, "counter");
  ASSERT_EQ(conf[0].exposed_, true);

  auto conf2 = concord::utils::ConcordBftMetricsManager::parseConfiguration(
      "resources/metrics_config.yaml");
  ASSERT_EQ(conf2[0].name_, "view");
  ASSERT_EQ(conf2[0].component_, "replica");
  ASSERT_EQ(conf2[0].type_, "gauge");
  ASSERT_EQ(conf2[0].exposed_, true);
}

TEST(utils_test, test_prometheus_create_families) {
  auto conf = std::vector<concord::utils::ConcordMetricConf>(
      {{"m1", {}, "counter", "", "", true}, {"m2", {}, "gauge", "", "", true}});
  auto pr = concord::utils::PrometheusRegistry("0.0.0.0:9891", conf);
  auto& cf = pr.createCounterFamily("m1", "h1", {});
  auto& counter = pr.createCounter(cf, {{"l1", "v1"}});
  counter.Increment();
  ASSERT_EQ(counter.Value(), 1);
  auto& gf = pr.createGaugeFamily("m2", "h2", {});
  auto& gauge = pr.createGauge(gf, {{"l1", "v1"}});
  gauge.Set(100);
  ASSERT_EQ(gauge.Value(), 100);
}

TEST(utils_test, test_prometheus_create) {
  auto conf = std::vector<concord::utils::ConcordMetricConf>(
      {{"m1", {}, "counter", "", "", true}, {"m2", {}, "gauge", "", "", true}});
  auto pr = concord::utils::PrometheusRegistry("0.0.0.0:9891", conf);
  auto& counter = pr.createCounter("m1", "h1", {{"l1", "v1"}});
  counter.Increment();
  ASSERT_EQ(counter.Value(), 1);
  auto& gauge = pr.createGauge("m2", "h2", {{"l1", "v1"}});
  gauge.Set(100);
  ASSERT_EQ(gauge.Value(), 100);
}

TEST(utils_test, test_prometheus_with_concord_bft_metrics) {
  auto conf = std::vector<concord::utils::ConcordMetricConf>(
      {{"m1", {{"l1", "v1"}}, "counter", "c1", "d1", true},
       {"m2", {{"l2", "v2"}}, "gauge", "c1", "d2", true}});
  auto pr = concord::utils::PrometheusRegistry("0.0.0.0:9891", {});
  auto cbft = concord::utils::ConcordBftMetricsManager(conf);
  concordMetrics::Component c1("c1", cbft.getAggregator());
  auto counter = c1.RegisterCounter("m1");
  auto gauge = c1.RegisterGauge("m2", 0);
  c1.Register();
  counter.Get().Inc();
  gauge.Get().Set(100);
  c1.UpdateAggregator();
  pr.scrapeRegistry(cbft.getCollector());
  for (auto& mf : cbft.getCollector()->Collect()) {
    if (mf.type == prometheus::MetricType::Counter) {
      ASSERT_EQ(mf.name, "concord_concordbft_m1");
      ASSERT_EQ(mf.metric[0].counter.value, 1);
    }
    if (mf.type == prometheus::MetricType::Gauge) {
      ASSERT_EQ(mf.name, "concord_concordbft_m2");
      ASSERT_EQ(mf.metric[0].gauge.value, 100);
    }
  }
}

}  // namespace

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
