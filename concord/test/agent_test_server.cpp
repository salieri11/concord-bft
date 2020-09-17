// Copyright 2020 VMware, all rights reserved

#include <nlohmann/json.hpp>
#include "Logger.hpp"
#include "utils/httplib/httplib.h"

using namespace httplib;
using json = nlohmann::json;

int main(void) {
  auto log = logging::getLogger("test.agent.server");
  Server svr;

  // Current version
  svr.Get("/releases/current", [&log](const Request& req, Response& res) {
    LOG_INFO(log, "/releases/current");
    json j = {{"version", "CURRENT_VERSION"}};
    res.set_content(j.dump(), "application/json");
  });

  // Get status of version
  svr.Get(R"(/releases/(\d+))", [&log](const Request& req, Response& res) {
    LOG_INFO(log, "/releases/" << req.matches[1]);
    json j = {{"version", req.matches[1]}, {"status", "VERSION_STATUS"}};
    res.set_content(j.dump(), "application/json");
  });

  // Download specific version
  svr.Put(R"(/releases/(\d+))", [&log](const Request& req, Response& res) {
    LOG_INFO(log, "/releases/" << req.matches[1]);
    json j = {{"version", req.matches[1]}, {"status", "DOWNLOAD_VERSION"}};
    res.set_content(j.dump(), "application/json");
  });

  svr.listen("localhost", 46347);
}
