// Copyright 2018-2019 VMware, all rights reserved

#include "concord_hlf_handler.hpp"

using Blockchain::Status;
using concord::blockchain::hlf::KvbStorageForHlf;

using log4cplus::Logger;

namespace concord {
namespace hlf {

concord::hlf::HlfHandler::HlfHandler(ChaincodeInvoker* chaincode_invoker)
    : chaincode_invoker_(chaincode_invoker),
      logger_(Logger::getInstance("com.vmware.concord.hlf.handler")) {}

concord::hlf::HlfHandler::~HlfHandler() {}

Status concord::hlf::HlfHandler::SetKvbHlfStoragePointer(
    KvbStorageForHlf* kvb_hlf_storage_instance) {
  kvb_hlf_storage_ = kvb_hlf_storage_instance;
  return Status::OK();
}

Status concord::hlf::HlfHandler::RevokeKvbHlfStoragePointer() {
  kvb_hlf_storage_ = nullptr;
  return Status::OK();
}

// APIS FOR HlfHandler KV SERVICE
Status concord::hlf::HlfHandler::PutState(string key, string value) {
  // check KvbStorageForHlf to avoid illeagal call
  if (!kvb_hlf_storage_) {
    return Status::IllegalOperation("KvbStorageForHlf instance not found");
  }

  const char* c = key.c_str();
  const uint8_t* p = reinterpret_cast<const uint8_t*>(c);
  kvb_hlf_storage_->SetHlfState(p, strlen(c), value);
  LOG4CPLUS_DEBUG(logger_, "Successfully set " << key << ":" << value);
  return Status::OK();
}

string concord::hlf::HlfHandler::GetState(string key) {
  if (!kvb_hlf_storage_) {
    LOG4CPLUS_ERROR(logger_, "KvbStorageForHlf instance not found");
    return "";
  }

  const char* c = key.c_str();
  const uint8_t* p = reinterpret_cast<const uint8_t*>(c);
  string value = kvb_hlf_storage_->GetHlfState(p, strlen(c));
  if (value != "") {
    LOG4CPLUS_DEBUG(logger_, "Successfully get " << key << ":" << value);
    return value;
  }

  return "";
}

// write concord block directly
Status concord::hlf::HlfHandler::WriteBlock() {
  if (!kvb_hlf_storage_) {
    return Status::IllegalOperation("KvbStorageForHlf instance not found");
  }

  // return status to client in gRPC module
  return kvb_hlf_storage_->WriteHlfBlock();
}

string concord::hlf::HlfHandler::GetConcordKvService() {
  if (chaincode_invoker_ != nullptr) {
    return chaincode_invoker_->GetHlfConcordKvServiceAddress();
  }
  return "";
}

// APIS FOR CHAINCODE INVOKER

// 1. install
Status concord::hlf::HlfHandler::InstallChaincode(string chaincode_name,
                                                  string path, string version) {
  int status = chaincode_invoker_->SendInstall(chaincode_name, path, version);
  if (!status) {
    return Status::OK();
  }
  return Status::IllegalOperation("Unable to install chaincode: " +
                                  chaincode_name);
}

// 2. instantiate
Status concord::hlf::HlfHandler::InstantiateChaincode(string chaincode_name,
                                                      string input,
                                                      string version) {
  int status =
      chaincode_invoker_->SendInstantiate(chaincode_name, input, version);
  if (!status) {
    return Status::OK();
  }
  return Status::IllegalOperation("Unable to instantiate chaincode: " +
                                  chaincode_name);
}

// 3. invoke
Status concord::hlf::HlfHandler::InvokeChaincode(string chaincode_name,
                                                 string input) {
  int status = chaincode_invoker_->SendInvoke(chaincode_name, input);
  if (!status) {
    return Status::OK();
  }
  return Status::IllegalOperation("Unable to invoke chaincode: " +
                                  chaincode_name);
}

// 4. query
string concord::hlf::HlfHandler::QueryChaincode(string chaincode_name,
                                                string input) {
  return chaincode_invoker_->SendQuery(chaincode_name, input);
}

// 5. upgrade
Status concord::hlf::HlfHandler::UpgradeChaincode(string chaincode_name,
                                                  string input,
                                                  string version) {
  int status = chaincode_invoker_->SendUpgrade(chaincode_name, input, version);
  if (!status) {
    return Status::OK();
  }
  return Status::IllegalOperation("Unable to upgrade chaincode: " +
                                  chaincode_name);
}

}  // namespace hlf
}  // namespace concord
