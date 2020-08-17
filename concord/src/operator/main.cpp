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
#include "httplib/httplib.h"

#include "Logger.hpp"
#include "bftclient/bft_client.h"
#include "communication/CommDefs.hpp"
#include "communication/CommFactory.hpp"
#include "kvstream.h"

#include "config.h"

using namespace httplib;
using namespace bft::communication;
using json = nlohmann::json;

static const char* CONFIG_FILE = "/operator/config-local/participant.config";

void startServer(bft::client::Client& client) {
  auto logger = logging::getLogger("concord.operator.server");
  Server svr;

  // Return the status of all releases
  svr.Get("/concord/releases", [](const Request& req, Response& res) {
    // TODO: Get actual values via BFT Client
    json installed;
    installed["version"] = 0.8;
    installed["status"] = "installed";

    json j = {{"replica0", installed},
              {"replica1", installed},
              {"replica2", installed},
              {"replica3", installed}};
    res.set_content(j.dump(), "application/json");
  });

  // Return the status of the given release
  svr.Get(R"(/concord/releases/(\d+))", [](const Request& req, Response& res) {
    // TODO: Get actual values via BFT Client
    auto version = req.matches[1];
    json j;
    j["version"] = version;
    if (version == "0.8") {
      j["status"] = "installed";
    } else {
      j["status"] = "does-not-exist";
    }
    json j2 = {
        {"replica0", j}, {"replica1", j}, {"replica2", j}, {"replica3", j}};

    res.set_content(j2.dump(), "application/json");
  });

  // Download all images for a given release
  svr.Put("/concord/releases", [&logger](const Request& req, Response& res) {
    // TODO: Trigger the download via BFT Client
    LOG_INFO(logger, "Downloading release: " << req.body);
  });

  // Trigger installation of a given release
  svr.Put("/concord/releases/install",
          [&logger](const Request& req, Response& res) {
            // TODO: Trigger install via BFT client
            LOG_INFO(logger, "Installing release: " << req.body);
          });

  // TODO: Make this part of operator config file?
  uint16_t port = 41444;
  LOG_INFO(logger, "HTTP Server listening on localhost:" << port);
  svr.listen("localhost", port);
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
  startServer(client);
  return 0;
}
