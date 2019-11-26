// Copyright 2019 VMware, all rights reserved

#include <grpcpp/grpcpp.h>
#include <log4cplus/configurator.h>
#include <log4cplus/logger.h>
#include <cstdlib>
#include "thin_replica_client.hpp"

using grpc::Status;
using log4cplus::Logger;

int main(int argc, char** argv) {
  log4cplus::initialize();
  if (const char* log_properties = std::getenv("LOG4CPLUS_CONFIGURATION")) {
    log4cplus::PropertyConfigurator::doConfigure(log_properties);
  } else {
    log4cplus::BasicConfigurator cfg;
    cfg.configure();
  }

  Logger logger(Logger::getInstance("thin_replica.example"));
  ThinReplicaClient trc(
      grpc::CreateChannel(argv[1], grpc::InsecureChannelCredentials()));

  Status status = trc.ReadState();
  if (!status.ok()) {
    LOG4CPLUS_WARN(logger, "Status(" << status.error_code() << ") "
                                     << status.error_message());
  }

  return 0;
}
