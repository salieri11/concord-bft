// Copyright 2018-2019 VMware, all rights reserved

#include "concord_hlf_chaincode_invoker.hpp"

using concord::config::ConcordConfiguration;
using log4cplus::Logger;
using std::string;

namespace concord {
namespace hlf {
ChaincodeInvoker::ChaincodeInvoker(ConcordConfiguration& node_config)
    : logger_(Logger::getInstance("com.vmware.concord.hlf.invoker")) {
  // Make sure all following parameters are existed

  hlf_peer_tool_ = node_config.getValue<string>("hlf_peer_command_tool_path");
  hlf_peer_tool_config_ =
      node_config.getValue<string>("hlf_peer_command_tool_config_path");
  hlf_peer_address_ = node_config.getValue<string>("hlf_peer_address");
  hlf_orderer_address_ = node_config.getValue<string>("hlf_orderer_address");
  hlf_local_msp_id_ = node_config.getValue<string>("hlf_peer_msp_id");
  hlf_local_msp_dir_ = node_config.getValue<string>("hlf_peer_msp_dir_path");
  hlf_concord_kv_service_address_ =
      node_config.getValue<string>("hlf_concord_kv_service_address");

  LOG4CPLUS_INFO(
      logger_,
      "Got peer command tool path: "
          << hlf_peer_tool_ << "Got config of peer command tool path: "
          << hlf_peer_tool_config_ << "Got peer address: " << hlf_peer_address_
          << "Got orderer address: " << hlf_orderer_address_
          << "Got local msp id: " << hlf_local_msp_id_
          << "Got local msp dir: " << hlf_local_msp_dir_
          << "Got concord kv service: " << hlf_concord_kv_service_address_);
}

ChaincodeInvoker::ChaincodeInvoker(string hlf_peer_tool_Path)
    : hlf_peer_tool_(hlf_peer_tool_Path),
      logger_(Logger::getInstance("com.vmware.concord.hlf.invoker")) {}

ChaincodeInvoker::~ChaincodeInvoker() {}

string ChaincodeInvoker::SubProcess(string cmd) {
  FILE* fp;
  char output[1024];
  string result = "";

  // catch stderr
  cmd = ConstructCmdPrefix() + cmd;
  cmd += " 2>&1";

  fp = popen(cmd.c_str(), "r");

  if (fp == nullptr) {
    LOG4CPLUS_ERROR(logger_, "Failed to execute command: " << cmd);
    return result;
  }

  while (fgets(output, sizeof(output) - 1, fp) != nullptr) {
    result += string(output);
  }

  // Maybe we should check state before return
  fclose(fp);
  return result;
}

int ChaincodeInvoker::SetHlfPeerTool(string hlf_peer_tool_path) {
  hlf_peer_tool_ = hlf_peer_tool_path;
  return 0;
}

int ChaincodeInvoker::SetHlfConcordKvServiceAddress(string kv_service_address) {
  hlf_concord_kv_service_address_ = kv_service_address;
  return 0;
}

string ChaincodeInvoker::GetHlfPeerTool() const { return hlf_peer_tool_; }

// params
// chaincode: chaincode to call
// input: args for calling chaincode
// return "" if any error happened during the SendQuery
string ChaincodeInvoker::SendQuery(string chaincode_name, string input) {
  // hardcoded channel name
  // we need "\'" here
  string command = hlf_peer_tool_ + " chaincode query  -C mychannel -n " +
                   chaincode_name + " -c " + "\'" + input + "\'";

  return SubProcess(command);
}

int ChaincodeInvoker::SendInvoke(string chaincode_name, string input) {
  // invoke won't Send any message to orderer, we keep it here
  // to avoid modifying the API
  string command = hlf_peer_tool_ + " chaincode invoke -o " +
                   hlf_orderer_address_ + " -C mychannel -n " + chaincode_name +
                   " -c " + "\'" + input + "\'";

  // parse output before return
  // log the details of invoke
  string result = SubProcess(command);

  // should be logged
  LOG4CPLUS_INFO(logger_, "Result from chaincode invoke" << result);

  if (result.find("200") != string::npos) {
    return 0;
  } else {
    // invalid
    return 1;
  }
}

int ChaincodeInvoker::SendInstall(string chaincode_name, string path,
                                  string version) {
  string command = hlf_peer_tool_ + " chaincode install -n " + chaincode_name +
                   " -p" + path + " -v" + version;
  string result = SubProcess(command);

  // should be logged
  LOG4CPLUS_INFO(logger_, "Result from chaincode install" << result);

  if (result.find("200") != string::npos) {
    return 0;
  } else {
    // invalid
    return 1;
  }
}

int ChaincodeInvoker::SendInstantiate(string chaincode_name, string input,
                                      string version) {
  // instantiate need orderer address

  string command = hlf_peer_tool_ + " chaincode instantiate -C mychannel -o " +
                   hlf_orderer_address_ + " -n " + chaincode_name + " -c " +
                   "\'" + input + "\'" + " -v " + version;
  string result = SubProcess(command);
  // should be logged
  LOG4CPLUS_INFO(logger_, "Result from chaincode instantiate" << result);

  if (result.find("200") != string::npos) {
    return 0;
  } else {
    // invalid
    return 1;
  }
}

int ChaincodeInvoker::SendUpgrade(string chaincode_name, string input,
                                  string version) {
  // upgrade need orderer address

  string command = hlf_peer_tool_ + " chaincode upgrade -C mychannel -o " +
                   hlf_orderer_address_ + " -n " + chaincode_name + " -c " +
                   "\'" + input + "\'" + " -v " + version;

  string result = SubProcess(command);
  // should be logged
  LOG4CPLUS_INFO(logger_, "Result from chaincode upgrade" << result);

  if (result.find("200") != string::npos) {
    return 0;
  } else {
    // invalid
    return 1;
  }
}

string ChaincodeInvoker::ConstructCmdPrefix() {
  string prefix = "";

  prefix += "FABRIC_CFG_PATH=" + hlf_peer_tool_config_;
  prefix += " CORE_PEER_ADDRESS=" + hlf_peer_address_;
  prefix += " CORE_PEER_LOCALMSPID=" + hlf_local_msp_id_;
  prefix += " CORE_PEER_MSPCONFIGPATH=" + hlf_local_msp_dir_;
  prefix += " CORE_CONCORD_ADDRESS=" + hlf_concord_kv_service_address_;

  return prefix + " ";
}

string ChaincodeInvoker::GetHlfConcordKvServiceAddress() const {
  return hlf_concord_kv_service_address_;
}

}  // namespace hlf
}  // namespace concord
