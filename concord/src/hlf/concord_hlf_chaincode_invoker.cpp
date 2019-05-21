// Copyright 2018-2019 VMware, all rights reserved

#include "concord_hlf_chaincode_invoker.hpp"

using concord::config::ConcordConfiguration;
using log4cplus::Logger;
using std::string;

concord::hlf::ChaincodeInvoker::ChaincodeInvoker(
    ConcordConfiguration &nodeConfig)
    : logger(Logger::getInstance("com.vmware.concord.hlf.invoker")) {
  // Make sure all following parameters are existed
  hlfPeerTool = nodeConfig.getValue<string>("hlf_peer_command_tool_path");
  LOG4CPLUS_INFO(logger, "Got peer command tool path: " << hlfPeerTool);

  hlfPeerToolConfig =
      nodeConfig.getValue<string>("hlf_peer_command_tool_config_path");
  LOG4CPLUS_INFO(logger,
                 "Got config of peer command tool path: " << hlfPeerToolConfig);

  hlfPeerAddress = nodeConfig.getValue<string>("hlf_peer_address");
  LOG4CPLUS_INFO(logger, "Got peer address: " << hlfPeerAddress);

  hlfOrdererAddress = nodeConfig.getValue<string>("hlf_orderer_address");
  LOG4CPLUS_INFO(logger, "Got orderer address: " << hlfOrdererAddress);

  hlfLocalMspId = nodeConfig.getValue<string>("hlf_peer_msp_id");
  LOG4CPLUS_INFO(logger, "Got local msp id: " << hlfLocalMspId);

  hlfLocalMspDir = nodeConfig.getValue<string>("hlf_peer_msp_dir_path");
  LOG4CPLUS_INFO(logger, "Got local msp dir: " << hlfLocalMspDir);

  hlfConcordKvServiceAddress =
      "0.0.0.0:" + nodeConfig.getValue<string>("hlf_concord_kv_service_port");

  LOG4CPLUS_INFO(logger,
                 "Got concord kv service: " << hlfConcordKvServiceAddress);
}

concord::hlf::ChaincodeInvoker::ChaincodeInvoker(string hlfPeerToolPath)
    : hlfPeerTool(hlfPeerToolPath),
      logger(Logger::getInstance("com.vmware.concord.hlf.invoker")) {}

concord::hlf::ChaincodeInvoker::~ChaincodeInvoker() {}

// function of system call
string concord::hlf::ChaincodeInvoker::systemCall(string cmd) {
  FILE *fp;
  char output[1024];
  string result = "";

  // catch stderr
  cmd = constructCmdPrefix() + cmd;
  cmd += " 2>&1";

  fp = popen(cmd.c_str(), "r");

  if (fp == nullptr) {
    LOG4CPLUS_ERROR(logger, "Failed to execute command: " << cmd);
    return result;
  }

  while (fgets(output, sizeof(output) - 1, fp) != nullptr) {
    result += string(output);
  }

  // Maybe we should check state before return
  fclose(fp);
  return result;
}

int concord::hlf::ChaincodeInvoker::setHlfPeerTool(string hlfPeerToolPath) {
  hlfPeerTool = hlfPeerToolPath;
  return 0;
}

int concord::hlf::ChaincodeInvoker::setHlfConcordKvServiceAddress(
    string kvServiceAddress) {
  hlfConcordKvServiceAddress = kvServiceAddress;
  return 0;
}

string concord::hlf::ChaincodeInvoker::getHlfPeerTool() const {
  return hlfPeerTool;
}

// params
// chaincode: chaincode to call
// input: args for calling chaincode
// return "" if any error happened during the sendQuery
string concord::hlf::ChaincodeInvoker::sendQuery(string chaincodeName,
                                                 string input) {
  // hardcoded channel name
  // we need "\'" here
  string command = hlfPeerTool + " chaincode query  -C mychannel -n " +
                   chaincodeName + " -c " + "\'" + input + "\'";

  return systemCall(command);
}

int concord::hlf::ChaincodeInvoker::sendInvoke(string chaincodeName,
                                               string input) {
  // invoke won't send any message to orderer, we keep it here
  // to avoid modifying the API
  string command = hlfPeerTool + " chaincode invoke -o " + hlfOrdererAddress +
                   " -C mychannel -n " + chaincodeName + " -c " + "\'" + input +
                   "\'";

  // parse output before return
  // log the details of invoke
  string result = systemCall(command);

  // should be logged
  LOG4CPLUS_INFO(logger, "Result from chaincode invoke" << result);

  if (result.find("200") != string::npos) {
    return 0;
  } else {
    // invalid
    return 1;
  }
}

int concord::hlf::ChaincodeInvoker::sendInstall(string chaincodeName,
                                                string path, string version) {
  string command = hlfPeerTool + " chaincode install -n " + chaincodeName +
                   " -p" + path + " -v" + version;
  string result = systemCall(command);

  // should be logged
  LOG4CPLUS_INFO(logger, "Result from chaincode install" << result);

  if (result.find("200") != string::npos) {
    return 0;
  } else {
    // invalid
    return 1;
  }
}

int concord::hlf::ChaincodeInvoker::sendInstantiate(string chaincodeName,
                                                    string input,
                                                    string version) {
  // instantiate need orderer address

  string command = hlfPeerTool + " chaincode instantiate -C mychannel -o " +
                   hlfOrdererAddress + " -n " + chaincodeName + " -c " + "\'" +
                   input + "\'" + " -v " + version;
  string result = systemCall(command);
  // should be logged
  LOG4CPLUS_INFO(logger, "Result from chaincode instantiate" << result);

  if (result.find("200") != string::npos) {
    return 0;
  } else {
    // invalid
    return 1;
  }
}

int concord::hlf::ChaincodeInvoker::sendUpgrade(string chaincodeName,
                                                string input, string version) {
  // upgrade need orderer address

  string command = hlfPeerTool + " chaincode upgrade -C mychannel -o " +
                   hlfOrdererAddress + " -n " + chaincodeName + " -c " + "\'" +
                   input + "\'" + " -v " + version;

  string result = systemCall(command);
  // should be logged
  LOG4CPLUS_INFO(logger, "Result from chaincode upgrade" << result);

  if (result.find("200") != string::npos) {
    return 0;
  } else {
    // invalid
    return 1;
  }
}

string concord::hlf::ChaincodeInvoker::constructCmdPrefix() {
  string prefix = "";

  prefix += "FABRIC_CFG_PATH=" + hlfPeerToolConfig;
  prefix += " CORE_PEER_ADDRESS=" + hlfPeerAddress;
  prefix += " CORE_PEER_LOCALMSPID=" + hlfLocalMspId;
  prefix += " CORE_PEER_MSPCONFIGPATH=" + hlfLocalMspDir;
  prefix += " CORE_CONCORD_ADDRESS=" + hlfConcordKvServiceAddress;

  return prefix + " ";
}

string concord::hlf::ChaincodeInvoker::getHlfConcordKvServiceAddress() const {
  return hlfConcordKvServiceAddress;
}
