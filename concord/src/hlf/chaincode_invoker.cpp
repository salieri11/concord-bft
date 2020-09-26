// Copyright 2018-2019 VMware, all rights reserved

#include "hlf/chaincode_invoker.hpp"

using concord::config::ConcordConfiguration;
using concordUtils::Status;
using logging::Logger;
using std::endl;
using std::string;

namespace concord {
namespace hlf {
ChaincodeInvoker::ChaincodeInvoker(ConcordConfiguration& node_config)
    : logger_(Logger::getInstance("concord.hlf.invoker")) {
  // Make sure all following parameters exist

  hlf_peer_tool_ = node_config.getValue<string>("hlf_peer_command_tool_path");
  hlf_peer_tool_config_ =
      node_config.getValue<string>("hlf_peer_command_tool_config_path");
  hlf_peer_address_ = node_config.getValue<string>("hlf_peer_address");
  hlf_orderer_address_ = node_config.getValue<string>("hlf_orderer_address");
  hlf_local_msp_id_ = node_config.getValue<string>("hlf_peer_msp_id");
  hlf_local_msp_dir_ = node_config.getValue<string>("hlf_peer_msp_dir_path");
  hlf_kv_service_address_ =
      node_config.getValue<string>("hlf_kv_service_address");

  LOG_INFO(logger_, "Got peer command tool path: "
                        << hlf_peer_tool_ << endl
                        << "Got config of peer command tool path: "
                        << hlf_peer_tool_config_ << endl
                        << "Got peer address: " << hlf_peer_address_ << endl
                        << "Got orderer address: " << hlf_orderer_address_
                        << endl
                        << "Got local msp id: " << hlf_local_msp_id_ << endl
                        << "Got local msp dir: " << hlf_local_msp_dir_ << endl
                        << "Got concord kv service: " << hlf_kv_service_address_
                        << endl);
}

ChaincodeInvoker::ChaincodeInvoker(string hlf_peer_tool_Path)
    : hlf_peer_tool_(hlf_peer_tool_Path),
      logger_(Logger::getInstance("concord.hlf.invoker")) {}

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
    LOG_ERROR(logger_, "Failed to execute command: " << cmd);
    return result;
  }

  while (fgets(output, sizeof(output) - 1, fp) != nullptr) {
    result += string(output);
  }

  // TODO(lukec):
  // check the state before return.
  fclose(fp);
  return result;
}

Status ChaincodeInvoker::SetHlfPeerTool(string hlf_peer_tool_path) {
  hlf_peer_tool_ = hlf_peer_tool_path;
  return Status::OK();
}

Status ChaincodeInvoker::SetHlfKvServiceAddress(string kv_service_address) {
  hlf_kv_service_address_ = kv_service_address;
  return Status::OK();
}

string ChaincodeInvoker::GetHlfPeerTool() const { return hlf_peer_tool_; }

string ChaincodeInvoker::SendQuery(string chaincode_name, string input) {
  // hardcoded channel name and we need "\'" here
  string command = hlf_peer_tool_ + " chaincode query  -C mychannel -n " +
                   chaincode_name + " -c " + "\'" + input + "\'";

  return SubProcess(command);
}

Status ChaincodeInvoker::SendInvoke(string chaincode_name, string input) {
  // invoke won't Send any message to orderer, we keep it here
  // to avoid modifying the API
  string command = hlf_peer_tool_ + " chaincode invoke -o " +
                   hlf_orderer_address_ + " -C mychannel -n " + chaincode_name +
                   " -c " + "\'" + input + "\'";

  string result = SubProcess(command);

  LOG_DEBUG(logger_, "Result from chaincode invoke" << result);

  if (result.find("200") != string::npos) {
    return Status::OK();
  } else {
    // invalid
    return Status::NotFound("Failed to execute chaincode");
  }
}

Status ChaincodeInvoker::SendInstall(string chaincode_name, string path,
                                     string version) {
  string command = hlf_peer_tool_ + " chaincode install -n " + chaincode_name +
                   " -p" + path + " -v" + version;
  string result = SubProcess(command);

  // should be logged
  LOG_DEBUG(logger_, "Result from chaincode install" << result);

  if (result.find("200") != string::npos) {
    return Status::OK();
  } else {
    // invalid
    return Status::NotFound("Failed to execute chaincode");
  }
}

Status ChaincodeInvoker::SendInstantiate(string chaincode_name, string input,
                                         string version) {
  // instantiate need orderer address

  string command = hlf_peer_tool_ + " chaincode instantiate -C mychannel -o " +
                   hlf_orderer_address_ + " -n " + chaincode_name + " -c " +
                   "\'" + input + "\'" + " -v " + version;
  string result = SubProcess(command);
  LOG_DEBUG(logger_, "Result from chaincode instantiate" << result);

  if (result.find("200") != string::npos) {
    return Status::OK();
  } else {
    // invalid
    return Status::NotFound("Failed to execute chaincode");
  }
}

Status ChaincodeInvoker::SendUpgrade(string chaincode_name, string input,
                                     string version) {
  // upgrade need orderer address

  string command = hlf_peer_tool_ + " chaincode upgrade -C mychannel -o " +
                   hlf_orderer_address_ + " -n " + chaincode_name + " -c " +
                   "\'" + input + "\'" + " -v " + version;

  string result = SubProcess(command);
  LOG_DEBUG(logger_, "Result from chaincode upgrade" << result);

  if (result.find("200") != string::npos) {
    return Status::OK();
  } else {
    // invalid
    return Status::NotFound("Failed to execute chaincode");
  }
}

string ChaincodeInvoker::ConstructCmdPrefix() {
  string prefix = "";

  prefix += "FABRIC_CFG_PATH=" + hlf_peer_tool_config_;
  prefix += " CORE_PEER_ADDRESS=" + hlf_peer_address_;
  prefix += " CORE_PEER_LOCALMSPID=" + hlf_local_msp_id_;
  prefix += " CORE_PEER_MSPCONFIGPATH=" + hlf_local_msp_dir_;
  prefix += " CORE_CONCORD_ADDRESS=" + hlf_kv_service_address_;

  return prefix + " ";
}

string ChaincodeInvoker::GetHlfKvServiceAddress() const {
  return hlf_kv_service_address_;
}

}  // namespace hlf
}  // namespace concord
