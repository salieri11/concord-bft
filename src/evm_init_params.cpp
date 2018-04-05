// Copyright 2018 VMware, all rights reserved
//
// Athena Ethereum VM management.

#include<iostream>
#include "evm_init_params.hpp"

using namespace com::vmware::athena;
using log4cplus::Logger;
using json = nlohmann::json;

com::vmware::athena::EVMInitParams::EVMInitParams()
   : logger(Logger::getInstance("com.vmware.athena.evm_init_params")) {}

com::vmware::athena::EVMInitParams::EVMInitParams(std::string genesis_file_path)
   : logger(Logger::getInstance("com.vmware.athena.evm_init_params")) {
   json genesis_block = parse_genesis_block(genesis_file_path);

   if (genesis_block.find("config") != genesis_block.end()) {
      json config = genesis_block["config"];
      if (config.find("chainId") != config.end())
         chainID = config["chainId"];
   }
   LOG4CPLUS_INFO(logger, "Connecting to Chain " << chainID);

   if (genesis_block.find("alloc") != genesis_block.end()) {
      json alloc = genesis_block["alloc"];
      for (json::iterator it = alloc.begin();
           it != alloc.end(); it++) {
         std::vector<uint8_t> address = dehex(it.key());
         std::string balance_str = it.value()["balance"];
         // The second argument to stoull is not used (hence nullptr)
         // '0' for third parameter suggests that interpret base from string

         // Note: std::stoull tries to interpret base from string. However, if string is a vaild
         // number (within uint64_t range) but starts with '0' (it means its a octal number)
         // and has digit '9' then std::stoull won't throw any error instead it will return 0 as
         // balance value.
         uint64_t balance = 0;
         try {
            balance = std::stoull(balance_str, nullptr, 0);
         } catch (std::invalid_argument &ex) {
            LOG4CPLUS_ERROR(logger, "Invalid format for account balance value: "
                            << ex.what());
            throw EVMInitParamException("Invalid 'alloc' section in genesis file");
         } catch (std::out_of_range &ex) {
            LOG4CPLUS_ERROR(logger, "Account balance value out of range (should fit in uint64_t): "
                            << ex.what());
            throw EVMInitParamException("Invalid 'alloc' section in genesis file");
         }
         initial_accounts[address] = balance;
         LOG4CPLUS_TRACE(logger, "New initial account added with balance: " << balance);
      }
   }
   LOG4CPLUS_INFO(logger, initial_accounts.size() << " initial accounts added.");
}

/**
   Reads the genesis block json from file @genesis_file_path.
   This json is parsed and converted into nlohmann::json and returned
 */
json com::vmware::athena::EVMInitParams::parse_genesis_block(std::string genesis_file_path) {
   std::ifstream genesis_stream(genesis_file_path);
   json genesis_block;
   if (genesis_stream.good()) {
      genesis_stream >> genesis_block;
   } else {
      LOG4CPLUS_ERROR(logger, "Error reading genesis file at " + genesis_file_path + " Exiting.");
      throw EVMInitParamException("Unable to read genesis file at: " + genesis_file_path);
   }
   return genesis_block;
}



std::map<std::vector<uint8_t>, uint64_t>
com::vmware::athena::EVMInitParams::get_initial_accounts() {
   return initial_accounts;
}

uint64_t com::vmware::athena::EVMInitParams::get_chainID() {
      return chainID;
}
