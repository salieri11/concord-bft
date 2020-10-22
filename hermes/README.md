# hermes
Hermes is the testing framework for VMW Blockchain.

## Goals:
- Tests are run from the hermes project.
- Test suites are not tied to a specific language or testing
  framework.

## Code organization:
- hermes/conftest.py: Defines fixtures used accross all pytests.
- hermes/fixtures/common_fixtures.py: Place for common fixtures.
- hermes/resources: Configuration files.
- hermes/rpc: RPC calls.
- hermes/suites: Test suite classes.
- hermes/suites/skipped: Test cases to skip.  A suite has its own file(s).
- hermes/suites/supplementalResults: Expected results to use when they are
  missing from Ethereum's VM tests.
- hermes/util: Other source code files.  When a new category of files becomes
  apparent, move those files from util to a new directory.

## How to add a test suite:
Pytest:
- Pytest doc: https://docs.pytest.org/en/latest/
- Create new suites using pytest.  See "SampleSuite" as a basic, functional
  example.
- Add a friendly, human readable name to the "suites" in _get_suite_short_name
  defined in conftest.py
- Add a mapping of the friendly name to your pytest file.
  e.g.
  ```
    suite_list = {
        "CastorDeploymentTests": "hermes.suites.castor_deployment_tests",
        "SampleSuite": "hermes.suites.sample_suite",
    }
  ```
- Write your test cases in your pytest file.



## Requirements:
- Python 3.7.1.  Installation reference: https://docs.python.org/3/using/
- Python packages: matplotlib, numpy, pyyaml, pytest==5.3.5, pytest-json, web3,
  and xvfbwrapper. Pip may be fussy if Python on the system was upgraded it's
  easier to use virtualenv (see below).  Try:
  `python3 -m pip install <package>` if having trouble.
- To run the "EthCoreVmTests" suite, fetch the ethereum/tests project from Github:
  https://github.com/ethereum/tests
  Then adjust the path to it in resources/user_config.json.
- Install Maven: `sudo apt-get install maven`
- For the UITests to pass you need to export your LINT_API_KEY and FLUENTD key, which can be (found here)[https://console.cloud.vmware.com/csp/gateway/portal/#/user/tokens]

```shell
export LINT_API_KEY=XXXXXXXXX
```

Update Fluentd Key in `docker/fluentd/fluentd.conf` here

```shell
Authorization Bearer <ADD-LOGINTELLIGENCE-KEY-HERE>
```


## Running a test suite:
- Build the product into docker images. See (../README.md) for instructions.
- Run `python -m pytest suites/sample_suite.py`
- Hermes will use docker-compose to launch the product. The default tag it tries to look for is
  \<component\>:latest.
- The docker tag is defined in a file called .env.  To use a differently tagged docker image,
  edit the .env file in the hermes directory.  If it is not present there, edit the one in
  the ../docker directory.  (Hermes simply copies the one from ../docker to hermes
  if it is not found in the hermes directory.)
- Hermes can run Concord's configuration generation to generate new
  configuration files prior to launching tests; however, it does not do so by
  default. The `--runConcordConfigurationGeneration` option must be given to
  Hermes to run Concord configuration generation, otherwise, Hermes will skip
  this step and run with the existing configuration files. Note that this option
  will be used during automated testing when Hermes is run by CI to verify that
  new commits do not break the configuration system.

## Running a test suite in Python Virtual Environment
- Intall the python virtual environment, you could choose anyone you like, here we use Miniconda for example,

  ```
  wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
  bash Miniconda3-latest-Linux-x86_64.sh
  ```
- Create a python environment for your project
  ```bash
  conda create -n hermes python=3.7
  ```
  You can list all your virtual environments
  ```bash
  conda info --envs
  ```
  You should be able to see the newly created virtual environment `hermes`.
  For more detail about how to use `conda`, please refer [Getting started with conda](https://conda.io/docs/user-guide/getting-started.html#getting-started-with-conda)

- Change to your blockchain/hermes directory and activate you new created virtual environment
  ```bash
  cd your-blockchain-hermes-dir
  source activate hermes
  ```
- Install all the dependencies for hermes project
    ```bash
    pip install -r requirements.txt
    ```
- Locate the path of python executable of your virtual environment
    ```bash
    which python
    ```


All set! Run hermes as usual.

## Running Sample Test suite:
- Sample test suite is a demo test and helps in checking the environment setup
- Usage:
```
herme$ python -m pytest suites/sample_suite.py
```

- Result:
```
2020-10-22 23:39:14 INFO Setting up Hermes run time settings for SampleSuite Test Suite
2020-10-22 23:39:14 INFO Setting up Hermes for 'test_example' of 'SampleSuite'
2020-10-22 23:39:14 INFO Setting up Hermes for 'test_example_with_fixture' of 'SampleSuite'
2020-10-22 23:39:14 INFO Setting up Hermes for 'test_parameterize_example[stop_primary_node-stop_non_primary_node]' of 'SampleSuite'
2020-10-22 23:39:14 INFO Demo... stop_primary_node
2020-10-22 23:39:14 INFO Push transactions...now!!
2020-10-22 23:39:14 INFO Demo.. stop_non_primary_node(2)
2020-10-22 23:39:14 INFO Push transactions...again!!
2020-10-22 23:39:14 INFO Setting up Hermes for 'test_parameterize_example[stop_non_primary_node-stop_primary_node]' of 'SampleSuite'
2020-10-22 23:39:14 INFO Demo.. stop_non_primary_node(2)
2020-10-22 23:39:14 INFO Push transactions...now!!
2020-10-22 23:39:14 INFO Demo... stop_primary_node
2020-10-22 23:39:14 INFO Push transactions...again!!
2020-10-22 23:39:14 INFO Setting up Hermes for 'test_parameterize_example[crash_primary-crash_non_primary]' of 'SampleSuite'
2020-10-22 23:39:14 INFO Demo... crash_primary
2020-10-22 23:39:14 INFO Push transactions...now!!
2020-10-22 23:39:14 INFO Demo.... crash non primary
2020-10-22 23:39:14 INFO Push transactions...again!!
2020-10-22 23:39:14 INFO Setting up Hermes for 'test_parameterize_example[crash_non_primary-crash_primary]' of 'SampleSuite'
2020-10-22 23:39:14 INFO Demo.... crash non primary
2020-10-22 23:39:14 INFO Push transactions...now!!
2020-10-22 23:39:14 INFO Demo... crash_primary
2020-10-22 23:39:14 INFO Push transactions...again!!
2020-10-22 23:39:14 INFO Teardown module...
2020-10-22 23:39:14 INFO Teardown session...
2020-10-22 23:39:14 INFO Result file location: /tmp/execution_results.json
2020-10-22 23:39:14 INFO Result Summary: {'succeeded': 6, 'expected-failed': 0, 'skipped': 0, 'failed': 0, 'not-executed': 0, 'total': 6}
2020-10-22 23:39:14 INFO Test Result: Successfully completed test session...
2020-10-22 23:39:14 INFO Summary: [✔]tests succeeded 6, [✗]tests failed 0, tests skipped 0, tests expected-failed 0, tests not-executed 0, Total Tests 6
2020-10-22 23:39:14 INFO Complete Success? (True)
```

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
  "Results directory: /tmp/SampleSuite_20201022_233914/test_logs/"
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

- unintentionallySkippedTests.json: When a test was expected to run, but skipped,
  the name of the test and the reason are stored in this file.  Example reasons
  include missing expected results and a transaction not being mined within the
  allotted time.


## Contract Testing Tool
The createContract utility uses the libraries in the testing framework to give
you a handy way to create and execute bytecode.

### Usage:
```
createContract.py [-h] [--addPrefix] [--callIt] [--callData CALLDATA]
                  [--showStorage] [--storageIndices STORAGEINDICES]
                  [--returnIndices RETURNINDICES] [--ethereumMode]
                  [--logLevel LOGLEVEL]
                  bytecode

positional arguments:
  bytecode              Bytecode of the contract.

optional arguments:
  -h, --help            show this help message and exit
  --addPrefix           Add a prefix for the bytecode to be runnable. If you
                        don't use this, and your own bytecode does not have
                        some prefix code, your bytecode will be run once, when
                        the contract is created, and cannot be called again.
  --callIt              Create a second contract from which to call the
                        contract containing your bytecode, and call it. Not
                        implemented: Passing in a value for 'value'. NOTE:
                        Contracts are always executed once when intially
                        created. This argument is an additional call in order
                        to get the return value and pass in data with
                        --callData.
  --callData CALLDATA   Data to pass to the contract when using --callIt. In
                        the callee, this data is available as msg.data or
                        CALLDATALOAD (instruction 35).
  --showStorage         Display storage after execution. If --callIt is used,
                        then storage will be displayed twice: Once after
                        initial contract creation, and again after the
                        invocation.
  --storageIndices STORAGEINDICES
                        If using --showStorage, the number of storage
                        locations to display.
  --returnIndices RETURNINDICES
                        If using --callIt, the number of 32-byte chunks of
                        return data to display.
  --ethereumMode        Run against Ethereum instead of the product. Ethereum
                        should already be running and mining.
  --logLevel LOGLEVEL   Set the log level. Valid values:'DEBUG', 'INFO',
                        'WARNING', 'ERROR', 'CRITICAL'
```

### Testing Tool Example
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

## Old Performance Test suite:
THIS BLOCK SHOULD BE TESTED AND THEN MOVED TO THE TOP SECTION OF THIS PAGE

- The performance test utility assumes the format specified [here](https://vmwblockchain.atlassian.net/browse/ATH-4?filter=-5)
  for the test file.
- A sample test file can be downloaded from [here](http://pa-dbc1122.eng.vmware.com/bfink/vmwathena/test-data/NORMALIZED_COMMANDS.txt.gz)
- Note: The test file should be a *.txt.gz file and it should be present in the hermes root folder.
- The name of the file may be specified in resources/user_config.json under performance->filename.
- Install dependencies:
```
sudo apt-get install python3-matplotlib
sudo pip3 install numpy
```
- Usage:
```
python -m pytest suites/performance_tests.py
```
- To terminate the test early use command-C. This will stop the test and will only parse results attained up until that point.
