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

#include <memory>
#include <string>

#include <nlohmann/json.hpp>
#include "utils/httplib/httplib.h"

#include "Logger.hpp"
#include "bftclient/bft_client.h"
#include "communication/CommDefs.hpp"
#include "communication/CommFactory.hpp"
#include "kvstream.h"

#include "config.h"
#include "operations.hpp"

using namespace httplib;
using namespace bft::communication;
using json = nlohmann::json;

static const char* CONFIG_FILE = "/operator/config-local/operator.config";

void startServer(concord::op::Operations& ops) {
  auto logger = logging::getLogger("concord.operator.server");
  Server svr;

  // Return the status of all releases
  try {
    svr.Get("/concord/releases", [&ops](const Request& req, Response& res) {
      // TODO: Get actual values via BFT Client
      auto result = ops.initiateHasSwVersion(1s);
      json j = {{"succ", result.res.reconfiguration_sm_response().success()}};
      if (result.res.reconfiguration_sm_response().has_additionaldata()) {
        j["additional_data"] =
            result.res.reconfiguration_sm_response().additionaldata();
      }
      res.set_content(j.dump(), "application/json");
    });
  } catch (std::exception& e) {
    std::cerr << "An exception occurred while trying to svr.Get: "
                 "\"/concord/releases\" "
                 "exception message: "
              << e.what() << std::endl;
  }

  // Return the status of the given release
  try {
    svr.Get(R"(/concord/releases/(\d+))",
            [&logger](const Request& req, Response& res) {
              LOG_INFO(logger, "not implemented");
            });
  } catch (std::exception& e) {
    std::cerr << "An exception occurred while trying to svr.Get: "
                 "\"/concord/releases/(\\d+)\" "
                 "exception message: "
              << e.what() << std::endl;
  }

  // Download all images for a given release
  try {
    svr.Put("/concord/releases", [&ops](const Request& req, Response& res) {
      auto result = ops.initiateSwDownload(1s);
      json j = {{"succ", result.res.reconfiguration_sm_response().success()}};
      if (result.res.reconfiguration_sm_response().has_additionaldata()) {
        j["additional_data"] =
            result.res.reconfiguration_sm_response().additionaldata();
      }
      res.set_content(j.dump(), "application/json");
    });
  } catch (std::exception& e) {
    std::cerr << "An exception occurred while trying to svr.Put: "
                 "\"/concord/releases\" "
                 "exception message: "
              << e.what() << std::endl;
  }

  // Trigger installation of a given release
  try {
    svr.Put("/concord/releases/install", [&ops](const Request& req,
                                                Response& res) {
      auto result = ops.initiateInstallSwVersion(1s);
      json j = {{"succ", result.res.reconfiguration_sm_response().success()}};
      if (result.res.reconfiguration_sm_response().has_additionaldata()) {
        j["additional_data"] =
            result.res.reconfiguration_sm_response().additionaldata();
      }
      res.set_content(j.dump(), "application/json");
    });
  } catch (std::exception& e) {
    std::cerr << "An exception occurred while trying to svr.Put: "
                 "\"/concord/releases/intall\" "
                 "exception message: "
              << e.what() << std::endl;
  }
  try {
    svr.Get("/concord/wedge/status", [&ops, &logger](const Request& req,
                                                     Response& res) {
      auto timeout = std::stoi(req.params.find("timeout")->second);
      if (timeout <= 0) {
        LOG_WARN(logger,
                 "received invalid timeout, the request won't be executed"
                     << KVLOG(timeout));
        return;
      }
      concord::op::Response result =
          ops.WedgeStatus(std::chrono::seconds(timeout));
      json j;
      for (auto& rsi : result.rsis) {
        j[std::to_string(std::get<0>(rsi).val)] =
            std::get<1>(rsi).wedge_response().stopped() ? "true" : "false";
      }
      res.set_content(j.dump(), "application/json");
    });
  } catch (std::exception& e) {
    std::cerr << "An exception occurred while trying to svr.Get: "
                 "\"/concord/wedge/status\" "
                 "exception message: "
              << e.what() << std::endl;
  }
  try {
    svr.Put("/concord/wedge/stop", [&ops, &logger](const Request& req,
                                                   Response& res) {
      auto timeout = std::stoi(req.params.find("timeout")->second);
      if (timeout <= 0) {
        LOG_WARN(logger,
                 "received invalid timeout, the request won't be executed"
                     << KVLOG(timeout));
        return;
      }
      auto result = ops.initiateWedge(std::chrono::seconds(timeout));
      json j = {{"succ", result.res.reconfiguration_sm_response().success()}};
      if (result.res.reconfiguration_sm_response().has_additionaldata()) {
        j["additional_data"] =
            result.res.reconfiguration_sm_response().additionaldata();
      }
      res.set_content(j.dump(), "application/json");
    });
  } catch (std::exception& e) {
    std::cerr << "An exception occurred while trying to svr.Put: "
                 "\"/concord/wedge/stop\" "
                 "exception message: "
              << e.what() << std::endl;
  }

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
  auto comm = initComm(config);
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
