// Copyright 2018-2019 VMware, all rights reserved

#include "hlf/handler.hpp"

using concord::consensus::Status;

using log4cplus::Logger;

namespace concord {
namespace hlf {

HlfHandler::HlfHandler(ChaincodeInvoker* chaincode_invoker)
    : chaincode_invoker_(chaincode_invoker),
      logger_(Logger::getInstance("com.vmware.concord.hlf.handler")) {}

HlfHandler::~HlfHandler() {}

Status HlfHandler::SetKvbHlfStoragePointer(
    HlfKvbStorage* kvb_hlf_storage_instance) {
  kvb_hlf_storage_ = kvb_hlf_storage_instance;
  return Status::OK();
}

Status HlfHandler::RevokeKvbHlfStoragePointer() {
  kvb_hlf_storage_ = nullptr;
  return Status::OK();
}

Status HlfHandler::AddTransaction(
    const com::vmware::concord::HlfRequest& hlf_request) {
  // check HlfKvbStorage to avoid illeagal call
  if (!kvb_hlf_storage_) {
    return Status::IllegalOperation("HlfKvbStorage instance not found");
  }

  return kvb_hlf_storage_->AddHlfTransaction(hlf_request);
}

Status HlfHandler::PutState(string key, string value) {
  // check HlfKvbStorage to avoid illeagal call
  if (!kvb_hlf_storage_) {
    return Status::IllegalOperation("HlfKvbStorage instance not found");
  }

  // const char* c = key.c_str();
  // const uint8_t* p = reinterpret_cast<const uint8_t*>(c);
  kvb_hlf_storage_->SetHlfState(key, value);
  LOG4CPLUS_DEBUG(logger_, "Successfully set " << key << ":" << value);
  return Status::OK();
}

string HlfHandler::GetState(const string& key) {
  if (!kvb_hlf_storage_) {
    LOG4CPLUS_ERROR(logger_, "HlfKvbStorage instance not found");
    return "";
  }

  string value = kvb_hlf_storage_->GetHlfState(key);
  if (value != "") {
    LOG4CPLUS_DEBUG(logger_, "Successfully get " << key << ":" << value);
    return value;
  }

  return "";
}

// write concord block directly
Status HlfHandler::WriteBlock() {
  if (!kvb_hlf_storage_) {
    return Status::IllegalOperation("HlfKvbStorage instance not found");
  }

  // return status to client in gRPC module
  return kvb_hlf_storage_->WriteHlfBlock();
}

string HlfHandler::GetHlfKvService() {
  if (chaincode_invoker_ != nullptr) {
    return chaincode_invoker_->GetHlfKvServiceAddress();
  }
  return "";
}

// APIS FOR CHAINCODE INVOKER

// 1. install
Status HlfHandler::InstallChaincode(string chaincode_name, string path,
                                    string version) {
  Status status =
      chaincode_invoker_->SendInstall(chaincode_name, path, version);
  if (status.isOK()) {
    return Status::OK();
  }
  return Status::IllegalOperation("Unable to install chaincode: " +
                                  chaincode_name);
}

// 2. instantiate
Status HlfHandler::InstantiateChaincode(string chaincode_name, string input,
                                        string version) {
  Status status =
      chaincode_invoker_->SendInstantiate(chaincode_name, input, version);
  if (status.isOK()) {
    return Status::OK();
  }
  return Status::IllegalOperation("Unable to instantiate chaincode: " +
                                  chaincode_name);
}

// 3. invoke
Status HlfHandler::InvokeChaincode(string chaincode_name, string input) {
  Status status = chaincode_invoker_->SendInvoke(chaincode_name, input);
  if (status.isOK()) {
    return Status::OK();
  }
  return Status::IllegalOperation("Unable to invoke chaincode: " +
                                  chaincode_name);
}

// 4. query
string HlfHandler::QueryChaincode(string chaincode_name, string input) {
  return chaincode_invoker_->SendQuery(chaincode_name, input);
}

// 5. upgrade
Status HlfHandler::UpgradeChaincode(string chaincode_name, string input,
                                    string version) {
  Status status =
      chaincode_invoker_->SendUpgrade(chaincode_name, input, version);
  if (status.isOK()) {
    return Status::OK();
  }
  return Status::IllegalOperation("Unable to upgrade chaincode: " +
                                  chaincode_name);
}

}  // namespace hlf
}  // namespace concord
