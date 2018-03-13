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
- hermes/suites: Test suite classes.
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
  Then adjust the path to it in the config file.
## Running a test suite:
- There will eventually be a config file for build locations.  For now, the build
  directories are hard coded in resources/product_launch_config.json.
- Pull and build the API server at ~/source/vmwathena/helen.
- If you do not have it at this location, then you can link to it:
  cd ~ && mkdir source && cd source && ln -s $YOUR_PATH_TO_VMWATHENA vmwathena
- Create a debug build of P2_Blockchain, allowing for the default output
  directory. (~/builds/p2-blockchain/debug)
- Run `./main.py CoreVMTests`
