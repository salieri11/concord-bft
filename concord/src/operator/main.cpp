// Concord
//
// Copyright (c) 2020 VMware, Inc. All Rights Reserved.
//
// This product is licensed to you under the Apache 2.0 license (the "License").
// You may not use this product except in compliance with the Apache 2.0
// License.
//
// This product may include a number of subcomponents with separate copyright
// notices and license terms. Your use of these subcomponents is subject to the
// terms and conditions of the sub-component's license, as noted in the LICENSE
// file.

#include <iomanip>
#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <sstream>
#include <string>

#include "Logger.hpp"
#include "bftclient/bft_client.h"
#include "communication/CommDefs.hpp"
#include "communication/CommFactory.hpp"
#include "concord.cmf.hpp"
#include "config.h"
#include "kvstream.h"
#include "operations.hpp"
#include "utils/httplib/httplib.h"

using namespace httplib;
using namespace bft::communication;
using json = nlohmann::json;

static const char* CONFIG_FILE = "/operator/config-local/operator.config";

void startServer(concord::op::Operations& ops) {
  auto logger = logging::getLogger("concord.operator.server");
  Server svr;

  // Return the status of all releases
  svr.Get("/concord/releases", [&ops, &logger](const Request& req,
                                               Response& res) {
    // TODO: Get actual values via BFT Client
    try {
      auto result = ops.initiateHasSwVersion(1s);
      json j = {{"succ", result.res.reconfiguration_sm_response().success()}};
      if (result.res.reconfiguration_sm_response().has_additionaldata()) {
        j["additional_data"] =
            result.res.reconfiguration_sm_response().additionaldata();
      }
      res.set_content(j.dump(), "application/json");
    } catch (std::exception& e) {
      LOG_ERROR(logger,
                "An exception occurred while trying to svr.Get: "
                "\"/concord/releases\" "
                "exception message: "
                    << e.what());
      json j = {{"succ", false}, {"additional_data", std::string(e.what())}};
      res.set_content(j.dump(), "application/json");
    }
  });

  // Return the status of the given release
  svr.Get(R"(/concord/releases/(\d+))",
          [&logger](const Request& req, Response& res) {
            LOG_INFO(logger, "not implemented");
          });

  // Download all images for a given release
  svr.Put("/concord/releases", [&ops, &logger](const Request& req,
                                               Response& res) {
    try {
      std::string version = req.params.find("version")->second;
      // TODO: The version string should conform to a pattern (TBD)
      auto result = ops.initiateSwDownload(1s, version);
      json j = {{"succ", result.res.reconfiguration_sm_response().success()}};
      if (result.res.reconfiguration_sm_response().has_additionaldata()) {
        j["additional_data"] =
            result.res.reconfiguration_sm_response().additionaldata();
      }
      res.set_content(j.dump(), "application/json");
    } catch (std::exception& e) {
      LOG_ERROR(logger,
                "An exception occurred while trying to svr.Put: "
                "\"/concord/releases\" "
                "exception message: "
                    << e.what());
      json j = {{"succ", false}, {"additional_data", std::string(e.what())}};
      res.set_content(j.dump(), "application/json");
    }
  });

  // Trigger installation of a given release
  svr.Put("/concord/releases/install", [&ops, &logger](const Request& req,
                                                       Response& res) {
    try {
      std::string version = req.params.find("version")->second;
      // TODO: The version string should conform to a pattern (TBD)
      auto result = ops.initiateInstallSwVersion(1s, version);
      json j = {{"succ", result.res.reconfiguration_sm_response().success()}};
      if (result.res.reconfiguration_sm_response().has_additionaldata()) {
        j["additional_data"] =
            result.res.reconfiguration_sm_response().additionaldata();
      }
      res.set_content(j.dump(), "application/json");
    } catch (std::exception& e) {
      LOG_ERROR(logger,
                "An exception occurred while trying to svr.Put: "
                "\"/concord/releases/install\" "
                "exception message: "
                    << e.what());
      json j = {{"succ", false}, {"additional_data", std::string(e.what())}};
      res.set_content(j.dump(), "application/json");
    }
  });

  svr.Get("/concord/wedge/status", [&ops, &logger](const Request& req,
                                                   Response& res) {
    try {
      auto timeout = std::stoi(req.params.find("timeout")->second);
      if (timeout <= 0) {
        LOG_ERROR(logger,
                  "received invalid timeout, the request won't be executed"
                      << KVLOG(timeout));
        json j = {{"succ", false},
                  {"additional_data",
                   "received invalid timeout, the request won't be executed"}};
        res.set_content(j.dump(), "application/json");
        return;
      }
      concord::op::Response result =
          ops.WedgeStatus(std::chrono::seconds(timeout));
      json j;
      for (auto& rsi : result.rsis) {
        concord::messages::WedgeResponse wedge_response;
        auto& data = std::get<1>(rsi).data();
        concord::messages::deserialize(
            std::vector<uint8_t>(data.begin(), data.end()), wedge_response);
        j[std::to_string(std::get<0>(rsi).val)] =
            wedge_response.stopped ? "true" : "false";
      }
      res.set_content(j.dump(), "application/json");
      LOG_INFO(logger, "done running wedge status command");
    } catch (std::exception& e) {
      LOG_ERROR(logger,
                "An exception occurred while trying to svr.Get: "
                "\"/concord/wedge/status\" "
                "exception message: "
                    << e.what());
      json j = {{"succ", false}, {"additional_data", std::string(e.what())}};
      res.set_content(j.dump(), "application/json");
    }
  });

  svr.Put("/concord/wedge/stop", [&ops, &logger](const Request& req,
                                                 Response& res) {
    try {
      auto timeout = std::stoi(req.params.find("timeout")->second);
      if (timeout <= 0) {
        LOG_ERROR(logger,
                  "received invalid timeout, the request won't be executed"
                      << KVLOG(timeout));
        json j = {{"succ", false},
                  {"additional_data",
                   "received invalid timeout, the request won't be executed"}};
        res.set_content(j.dump(), "application/json");
        return;
      }
      auto result = ops.initiateWedge(std::chrono::seconds(timeout));
      json j = {{"succ", result.res.reconfiguration_sm_response().success()}};
      if (result.res.reconfiguration_sm_response().has_additionaldata()) {
        j["additional_data"] =
            result.res.reconfiguration_sm_response().additionaldata();
      }
      res.set_content(j.dump(), "application/json");
      LOG_INFO(logger, "done running wedge stop command");
    } catch (std::exception& e) {
      LOG_ERROR(logger,
                "An exception occurred while trying to svr.Put: "
                "\"/concord/wedge/stop\" "
                "exception message: "
                    << e.what());
      json j = {{"succ", false}, {"additional_data", std::string(e.what())}};
      res.set_content(j.dump(), "application/json");
    }
  });

  svr.Get("/concord/prune/latestPruneableBlock", [&ops, &logger](
                                                     const Request& req,
                                                     Response& res) {
    try {
      auto timeout = std::stoi(req.params.find("timeout")->second);
      if (timeout <= 0) {
        LOG_ERROR(logger,
                  "received invalid timeout, the request won't be executed"
                      << KVLOG(timeout));
        json j = {{"succ", false},
                  {"additional_data",
                   "received invalid timeout, the request won't be executed"}};
        res.set_content(j.dump(), "application/json");
        return;
      }
      concord::op::Response result =
          ops.latestPruneableBlock(std::chrono::milliseconds(timeout * 1000));
      json j;
      for (auto& rsi : result.rsis) {
        concord::messages::LatestPrunableBlock response_;
        auto data = std::get<1>(rsi).data();
        concord::messages::deserialize(
            std::vector<uint8_t>(data.begin(), data.end()), response_);
        auto sig_str =
            concord::op::Utils::stringToByteString(response_.signature);
        j[std::to_string(std::get<0>(rsi).val)] = {
            {"block_id", std::to_string(response_.block_id)},
            {"signautre", sig_str}};
      }
      res.set_content(j.dump(), "application/json");
      LOG_INFO(logger, "done running prunelatestPruneableBlock command");
    } catch (std::exception& e) {
      LOG_ERROR(logger,
                "An exception occurred while trying to svr.Get: "
                "\"/concord/prune/latestPruneableBlock\" "
                "exception message: "
                    << e.what());
      json j = {{"succ", false}, {"additional_data", std::string(e.what())}};
      res.set_content(j.dump(), "application/json");
    }
  });

  svr.Put("/concord/prune/execute", [&ops, &logger](const Request& req,
                                                    Response& res) {
    try {
      auto timeout = std::stoi(req.params.find("timeout")->second);
      if (timeout <= 0) {
        LOG_ERROR(logger,
                  "received invalid timeout, the request won't be executed"
                      << KVLOG(timeout));
        json j = {{"succ", false},
                  {"additional_data",
                   "received invalid timeout, the request won't be executed"}};
        res.set_content(j.dump(), "application/json");
        return;
      }
      concord::op::Response result =
          ops.latestPruneableBlock(std::chrono::seconds(timeout));
      std::vector<concord::messages::LatestPrunableBlock>
          latest_pruneable_blocks;
      for (auto& rsi : result.rsis) {
        concord::messages::LatestPrunableBlock response_;
        auto& data = std::get<1>(rsi).data();
        concord::messages::deserialize(
            std::vector<uint8_t>(data.begin(), data.end()), response_);
        latest_pruneable_blocks.push_back(response_);
      }

      result = ops.initiatePrune(std::chrono::seconds(timeout),
                                 latest_pruneable_blocks);

      json j = {{"succ", result.res.reconfiguration_sm_response().success()}};
      if (result.res.reconfiguration_sm_response().has_additionaldata()) {
        j["additional_data"] =
            result.res.reconfiguration_sm_response().additionaldata();
      }
      res.set_content(j.dump(), "application/json");
      LOG_INFO(logger, "done running prune/exeute command");
    } catch (std::exception& e) {
      LOG_ERROR(logger,
                "An exception occurred while trying to svr.Put: "
                "\"/concord/prune/execute\" "
                "exception message: "
                    << e.what());
      json j = {{"succ", false}, {"additional_data", std::string(e.what())}};
      res.set_content(j.dump(), "application/json");
    }
  });

  // TODO: Make this part of operator config file?
  uint16_t port = 41444;
  LOG_INFO(logger, "HTTP Server listening on localhost:" << port);
  try {
    svr.listen("localhost", port);
  } catch (std::exception& e) {
    std::cerr << "An exception occurred while trying to svr.listen: "
                 "\"localhost\" "
                 "exception message: "
              << e.what() << std::endl;
  }
}

std::unique_ptr<ICommunication> initComm(const concord::op::Config& config) {
  const auto c = config.comm_config;
  if (c.commType == "tls") {
    const auto tls_config =
        TlsTcpConfig{c.listenIp,
                     c.listenPort,
                     c.bufferLength,
                     c.nodes,
                     static_cast<std::int32_t>(c.maxServerId),
                     c.selfId,
                     c.certificatesRootPath,
                     c.cipherSuite,
                     c.statusCallback};
    return std::unique_ptr<ICommunication>{CommFactory::create(tls_config)};
  } else if (c.commType == "udp") {
    const auto udp_config =
        PlainUdpConfig{c.listenIp, c.listenPort, c.bufferLength,
                       c.nodes,    c.selfId,     c.statusCallback};
    return std::unique_ptr<ICommunication>{CommFactory::create(udp_config)};
  }
  auto logger = logging::getLogger("concord.operator");
  LOG_FATAL(logger, "Invalid Communication type: " << KVLOG(c.commType));
  throw std::invalid_argument{"Unknown communication module type=" +
                              c.commType};
}

int main(int argc, char** argv) {
  auto logger = logging::getLogger("concord.operator");
  auto config = concord::op::Config::parse(CONFIG_FILE);
  bft::client::Client client(initComm(config), config.client_config);
  concord::op::Operations ops(config, client);
  try {
    startServer(ops);
  } catch (std::exception& e) {
    std::cerr << "An exception occurred while trying to startServer "
                 "exception message: "
              << e.what() << std::endl;
  }
  return 0;
}
