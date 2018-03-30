// Copyright 2018 VMware, all rights reserved
//
// Athena common Utilities.


#include "utils.hpp"

using namespace std;
using json = nlohmann::json;


/**
   Reads the genesis block json from file @genesis_file_path.
   This json is parsed and converted into nlohmann::json and returned
 */
json com::vmware::athena::parse_genesis_block(string genesis_file_path) {
   ifstream genesis_stream(genesis_file_path);
   json genesis_block;
   genesis_stream >> genesis_block;
   return genesis_block;
}
