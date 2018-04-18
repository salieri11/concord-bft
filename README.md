# hermes
Hermes is the repository for the vmwareathena project's testing framework.

## Goals:
- Tests are run from the hermes project.
- Test suites are not tied to a specific language or testing
  framework.

## Code organization:
- hermes/main.py: The main script, which uses Python 3.  Can be executed
  directly.
- hermes/resources: Configuration files.
- hermes/rpc: RPC calls.
- hermes/suites: Test suite classes.
- hermes/suites/skipped: Test cases to skip.  A suite has its own file(s).
- hermes/suites/supplementalResults: Expected results to use when they are
  missing from Ethereum's VM tests.
- hermes/util: Other source code files.  When a new category of files becomes
  apparent, move those files from util to a new directory.

## How to add a test suite:
- Create a new class under suites.
- The new class should extend TestSuite and implement the abstract methods
  defined in it.
- In main.py:
  - Add an import statement for the new suite.
  - In createTestSuite(), add an entry to detect that your test suite has been
    requested, and return an instance of it.
- The new test suite should return the path to a JSON file with the following
  format:
```
{
  "suite-name": {
  "result": "pass" or "fail",
    "tests": {
      "test1-name": {
        "result": "pass" or "fail",
        "info": String of text containing error messages, stack traces, etc...
      },
      ...
    }
  }
}
```

## Requirements:
- Python 3
- To run the "CoreVMTests" suite, fetch the ethereum/tests project from Github:
  https://github.com/ethereum/tests
  Then adjust the path to it in resources/user_config.json.

## Running a test suite:
- There will eventually be a config file for build locations.  For now, the build
  directories are hard coded in resources/product_launch_config.json.
- Pull and build the API server at ~/source/vmwathena/helen.
- Copy the config.properties file of Helen (if using the java version) over to Hermes and make changes (if required).
- If you do not have it at this location, then you can link to it:
  cd ~ && mkdir source && cd source && ln -s $YOUR_PATH_TO_VMWATHENA vmwathena
- Create a debug build of P2_Blockchain, allowing for the default output
  directory. (~/builds/p2-blockchain/debug)
- Run `./main.py CoreVMTests`

## Launching geth:
- If running the test suite against geth as a reference implementation, you need
  to set it up and launch it.  Some default config files have been checked in.
- Usage:
  1. Copy resources/initial_geth_data_dir to some location which does not exist
     yet.
```
cp -r resources/initial_geth_data_dir ~/datadir
```
  2. Open ~/datadir/genesis.json and replace
     PUT_A_UNIQUE_NUMBER_FOR_YOUR_INSTANCE_HERE with a number that is unique
     for you. This is to prevent others from connecting to your blockchain.
  3. Initialize Ethereum:
```
geth --datadir ~/datadir init ~/datadir/genesis.json
```
  4. Start geth:
```
geth --datadir ~/datadir --rpc --rpcaddr "0.0.0.0" --rpcapi "admin,db,eth,net,web3,personal,miner" --vmdebug --verbosity 5 --nodiscover --netrestrict <ip>/8 console
```
  5. Start mining:
```
miner.start(2)
```
- Now you can run the test cases with --ethereumMode with no further setup.

## Results
- The results directory is output at the beginning of a test run.  e.g.
  "Results directory: /tmp/CoreVMTests_20180322_1437_80ccst1i"
- product_logs directory: Product logs.
- test_logs directory: One subdirectory is created for each test.  Each of these
  should contain whatever information will help triage a failure.  For example,
  for every RPC call, two files are created:

  1. foo.log: The curl command (so you can re-run it yourself) and curl's output
     for making the RPC call (to reflect things like a server being down).
  2. foo.json: The JSON response from the RPC call.

  There will be numbers in front of each RPC call.  This number matches the "id"
  field of the RPC call, which is incremented for each call to help with
  triaging.  There will usually be many eth_getTransactionReceipt calls when
  using --ethereumMode because it takes time to wait for a transaction to be
  mined.
- `<suitename>`.json: JSON file of test results for the suite run.  The json must
  be consistent across test suites.  Sample:
```
  $ cat coreVMTestResults.json
  {
      "CoreVMTests": {
          "result": "PASS",
          "tests": {
              "not2": {
                  "info": "Log: /tmp/CoreVMTests_20180322_1437_80ccst1i/test_logs/not2",
                  "result": "PASS"
              }
          }
      }
  }
```
- unintentionallySkippedTests.json: When a test was expected to run, but skipped,
  the name of the test and the reason are stored in this file.  Example reasons
  include missing expected results and a transaction not being mined within the
  allotted time.
