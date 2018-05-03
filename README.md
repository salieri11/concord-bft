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


## Contract Testing Tool
The createContract utility uses the libraries in the testing framework to give
you a handy way to create and execute bytecode.

### Options:
```
  bytecode              Bytecode of the contract.

optional arguments:
  -h, --help            show this help message and exit
  --addPrefix           Add a prefix for the bytecode to be runnable.If you
                        don't use this, and your own bytecode does not have
                        some prefix code, your bytecode will be run once, when
                        the contract is created, and cannot be called again.
  --callIt              Create a second contract from which to call the
                        contract containing your bytecode, and call it.
                        --returnIndices 32-byte chunks of the return value of
                        the call will be displayed. Not implemented: Passing
                        in a value for 'value'. NOTE: Contracts are always
                        executed once when intially created. This argument is
                        an additional call in order to get the return value and
                        pass in data with --callData.
  --callData CALLDATA   Data to pass to the contract when using --callIt.In
                        the callee, this data is available as msg.data.
  --showStorage         Display storage after execution. If --callIt is used,
                        then storage will be displayed twice: Once after
                        initial contract creation, and again after the
                        invocation.
  --storageIndices STORAGEINDICES
                        If using --showStorage, the number of storage
                        locations to display
  --returnIndices RETURNINDICES
                        If using --callIt, the number of 32-byte chunks of
                        return data to display
  --ethereumMode        Run against Ethereum instead of the product. Ethereum
                        should already be running and mining.
  --logLevel LOGLEVEL   Set the log level. Valid values:'DEBUG', 'INFO',
                        'WARNING', 'ERROR', 'CRITICAL'
```

### Tesing Tool Example
Bytecode to test: 6003600055600035600155600460005260206000F3

Store the number 3 in storage position 0:
```
60 03
60 00
55
```

Copy 32 bytes of passed in data, starting at 0, to storage position 1:
```
60 00
35
60 01
55
```

Store the number 4 in memory.
```
60 04
60 00
52
```

Return 32 bytes of memory, starting at memory position 0:
```
60 20
60 00
F3
```

Create and test that bytecode.
```
./createContract.py 6003600055600035600155600460005260206000F3 --callIt --addPrefix --showStorage --storageIndices 2 --callData 1234567890abcdef
Results directory: /tmp/createContract_20180503_1325_e0bfz6k0
Creating initial contract with wrapped bytecode.
Contract address: '0xB35B8B030A4BC592EA8CCF3684512CE083F108DC'
Storage at contract '0xB35B8B030A4BC592EA8CCF3684512CE083F108DC' after contract creation:
   0: 0x0000000000000000000000000000000000000000000000000000000000000000
   1: 0x0000000000000000000000000000000000000000000000000000000000000000
Creating the contract which will call the contract created earlier.
Calling the contract which will call the contract created earlier.
Storage at contract '0xB35B8B030A4BC592EA8CCF3684512CE083F108DC' after contract invocation:
   0: 0x0000000000000000000000000000000000000000000000000000000000000003  <--- Storing the number 3 worked.
   1: 0x1234567890ABCDEF000000000000000000000000000000000000000000000000  <--- Passed in args were able to be read.
Data returned by the contract (32 bytes at a time):
   0: 0x0000000000000000000000000000000000000000000000000000000000000004  <--- The bytecode was able to return a value.
```