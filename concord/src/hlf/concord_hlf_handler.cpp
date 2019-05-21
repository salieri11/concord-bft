// Copyright 2018-2019 VMware, all rights reserved

#include "concord_hlf_handler.hpp"

using Blockchain::Status;
using concord::blockchain::hlf::KVBHlfStorage;

using log4cplus::Logger;

concord::hlf::HlfHandler::HlfHandler(ChaincodeInvoker *chaincodeInvoker)
    : chaincodeInvoker_(chaincodeInvoker),
      logger_(Logger::getInstance("com.vmware.concord.hlf.handler")) {}

concord::hlf::HlfHandler::~HlfHandler() {}

Status concord::hlf::HlfHandler::setKVBHlfStoragePointer(
    KVBHlfStorage *kvbsInstance) {
  kvbHlfStorage_ = kvbsInstance;
  return Status::OK();
}

Status concord::hlf::HlfHandler::revokeKVBHlfStoragePointer() {
  kvbHlfStorage_ = nullptr;
  return Status::OK();
}

/*
 #### APIS FOR HlfHandler KV SERVICE ####
*/
Status concord::hlf::HlfHandler::putState(string key, string value) {
  // check KVBHlfStorage to avoid illeagal call
  if (!kvbHlfStorage_) {
    return Status::IllegalOperation("KVBHlfStorage instance not found");
  }

  const char *c = key.c_str();
  const uint8_t *p = reinterpret_cast<const uint8_t *>(c);
  kvbHlfStorage_->set_hlf_state(p, strlen(c), value);
  LOG4CPLUS_DEBUG(logger_, "Successfully set " << key << ":" << value);
  return Status::OK();
}

string concord::hlf::HlfHandler::getState(string key) {
  if (!kvbHlfStorage_) {
    LOG4CPLUS_ERROR(logger_, "KVBHlfStorage instance not found");
    return "";
  }

  const char *c = key.c_str();
  const uint8_t *p = reinterpret_cast<const uint8_t *>(c);
  string value = kvbHlfStorage_->get_hlf_state(p, strlen(c));
  if (value != "") {
    LOG4CPLUS_DEBUG(logger_, "Successfully get " << key << ":" << value);
    return value;
  }

  return "";
}

// write concord block directly
Status concord::hlf::HlfHandler::writeBlock() {
  if (!kvbHlfStorage_) {
    return Status::IllegalOperation("KVBHlfStorage instance not found");
  }

  // return status to client in gRPC module
  return kvbHlfStorage_->write_hlf_block();
}

string concord::hlf::HlfHandler::getConcordKvService() {
  if (chaincodeInvoker_ != nullptr) {
    return chaincodeInvoker_->getHlfConcordKvServiceAddress();
  }
  return "";
}

/*
#### END OF APIS EXPOSED TO HlfHandler KV SERVICE ####
*/

/*
#### APIS FOR CHAINCODE INVOKER ####
*/

// 1. install
Status concord::hlf::HlfHandler::installChaincode(string chaincodeName,
                                                  string path, string version) {
  int status = chaincodeInvoker_->sendInstall(chaincodeName, path, version);
  if (!status) {
    return Status::OK();
  }
  return Status::IllegalOperation("Unable to install chaincode: " +
                                  chaincodeName);
}

// 2. instantiate
Status concord::hlf::HlfHandler::instantiateChaincode(string chaincodeName,
                                                      string input,
                                                      string version) {
  int status =
      chaincodeInvoker_->sendInstantiate(chaincodeName, input, version);
  if (!status) {
    return Status::OK();
  }
  return Status::IllegalOperation("Unable to instantiate chaincode: " +
                                  chaincodeName);
}

// 3. invoke
Status concord::hlf::HlfHandler::invokeChaincode(string chaincodeName,
                                                 string input) {
  int status = chaincodeInvoker_->sendInvoke(chaincodeName, input);
  if (!status) {
    return Status::OK();
  }
  return Status::IllegalOperation("Unable to invoke chaincode: " +
                                  chaincodeName);
}

// 4. query
string concord::hlf::HlfHandler::queryChaincode(string chaincodeName,
                                                string input) {
  return chaincodeInvoker_->sendQuery(chaincodeName, input);
}

// 5. upgrade
Status concord::hlf::HlfHandler::upgradeChaincode(string chaincodeName,
                                                  string input,
                                                  string version) {
  int status = chaincodeInvoker_->sendUpgrade(chaincodeName, input, version);
  if (!status) {
    return Status::OK();
  }
  return Status::IllegalOperation("Unable to upgrade chaincode: " +
                                  chaincodeName);
}

/*
#### END OF APIS TO HANDLER CHAINCODE INVOKE  ####
*/
