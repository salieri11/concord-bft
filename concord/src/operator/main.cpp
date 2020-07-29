#include <nlohmann/json.hpp>
#include "Logger.hpp"
#include "httplib/httplib.h"

using namespace httplib;
using json = nlohmann::json;
int main(int argc, char** argv) {
  auto logger = logging::getLogger("concord.operator");
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

  svr.listen("localhost", 41444);
  return 0;
}
