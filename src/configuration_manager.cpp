// Copyright 2018 VMware, all rights reserved

#include "configuration_manager.hpp"

using namespace boost::program_options;

using namespace std;

// default IP on which to listen for client connections
static const string default_listen_ip = "0.0.0.0";

// default port on which to listen for client connections
static const short default_listen_port = 5458;

// default location of logging properties file
static const string default_log_props = "./resources/log4cplus.properties";

// default location of configuration file
static const string default_config_file = "./resources/athena.config";

// default period to check for logging properties changes (milliseconds)
static const int default_log_props_time_ms = 60000; // 60sec

// default implementation of blockchain storage
static const string default_blockchain_db_impl = "memory";

// default size of API worker thread pool
static const int default_api_worker_thread_pool_size = 3;

// default count of maximum transactions returned by transaction list query
static const int default_transaction_list_max_count = 10;


variables_map initialize_config(int argc, char **argv) {
   // A map to hold key-value pairs of all options
   variables_map options_map;

   // holds the value of configuration file for Athena
   // this is NOT same as logger configuration file. Logger
   // configuration file can be specified by command line options or
   // as a property in configuration file.
   string config_file;

   // Program options which are generic for most of the programs:
   // These are not available via configuration files
   // only allowed to be passed on command line
   options_description generic{"Generic Options"};
   generic.add_options()
      ("help,h",
       "Print this help message")
      ("config,c",
       value<string>(&config_file)->default_value(default_config_file),
       "Path for configuration file")
      ("debug",
      "Sleep for 20 seconds to attach debug");
      ;

   // The configuration parameters specific to this program
   // These can be provided in config file as well as on command line
   // If same parameter is provided in config file as well as on
   // command line, then command line value will take preference.
   // Since, we read command line options first all the parameters specified
   // in command line will be populated in varaiables_map first, if same option
   // is present in config file it will be read but won't be stored in
   // variables_map since that 'key' already has some valid 'value' (value provided on cmdline)
   options_description config{"Configuration Options"};
   config.add_options()
      ("ip",
       value<std::string>()->default_value(default_listen_ip),
       "IP on which to expose the service")
      ("port,p",
       value<short>()->default_value(default_listen_port),
       "Port on which to expose the service")
      ("logger_config",
       value<string>()->default_value(default_log_props),
       "Path to logging properties file")
      ("logger_reconfig_time",
       value<int>()->default_value(default_log_props_time_ms),
       "Interval (in ms) to check for updates to logging properties file")
      ("genesis_block",
       value<string>(),
       "Absolute path of file which contains genesis block json")
      ("blockchain_db_impl",
       value<string>()->default_value(default_blockchain_db_impl),
       "Name of the DB implementation backing the blockchain. "
       "Legal values: memory, rocksdb")
      ("blockchain_db_path",
       value<string>(),
       "Path to blockchain database storage")
      // TOD(BWF): these are required, but this file needs to be rearranged to
      // make that work
      ("SBFT.public",
       value<string>(),
       "Path to SBFT public config file")
      ("SBFT.replica",
       value<string>(),
       "Path to SBFT private replica config file")
      ("SBFT.client",
       value<string>(),
       "Path to SBFT private client config file")
      ("api_worker_pool_size",
       value<int>()->default_value(default_api_worker_thread_pool_size),
       "Number of threads to create for handling TCP connections")
      ("transaction_list_max_count",
       value<int>()->default_value(default_transaction_list_max_count),
       "Maximum transactions returned for a transaction list query");


   options_description all_options; // description of all options
   all_options.add(generic).add(config);

   // First we parse command line options and see if --help
   // options was provided. In this case we don't need to
   // go for parsing config file. Otherwise call notify
   // for command line options and move to parsing config file
   store(command_line_parser(argc, argv).
         options(all_options).run(), options_map);

   // If cmdline options specified --help then we don't want
   // to do further processing for command line or
   // config file options
   if (options_map.count("help")) {
      std::cout << "VMware Project Athena" << std::endl;
      std::cout << all_options << std::endl;
      return options_map;
   }

   // call notify after checking "help", so that required
   // parameters are not required to get help (this call throws an
   // exception to exit the program if any parameters are invalid)
   notify(options_map);

   // Parse config file and populate map with those parameters
   // provided in config file.
   ifstream ifs(config_file.c_str());
   if (!ifs) {
      cerr << "Can not open config file: " << config_file
           << "\n" << " Going ahead with only command line options\n";
   } else {
      auto parsed = parse_config_file(ifs, config);
      store(parsed, options_map);
      notify(options_map);
   }

   return options_map;
}
