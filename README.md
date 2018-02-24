# hermes
Hermes is the repository for the vmwareathena project's testing framework.

## Goals:
- All of the tests (for every vmwareathena subproject) can be run with one command from the hermes directory.
- Tests for a subproject can be run from the hermes directory.
- Tests for a subproject can be run from that subproject's directory.
- Test suites in subprojects are not tied to a specific language or testing framework.

## Code organization:
- hermes/src: The high level testing framework which can launch tests in other projects and gather/report the results.
- hermes/src/lib: Generic library functions which can be used by the main framework or a test suite.
- hermes/include: Include files for the code in hermes.

## How to add a test suite:
- Create .cpp and .h files for your test suite in hermes.
- Have the new test suite inherit from TestSuite base class.
- Implement the getName() function.  This just returns a string.
- Implement the run() function.  This launches the tests in the subproject and returns the results as JSON.  The command to launch the tests should be the same as the command to launch the tests from the subproject.
- Modify main.cpp such that a new instance of the test suite will be created when that name is passed in.
- In Makefile, create a target for the test suite.
- When the test suite is runnable and checked in, add the target to the run_all_tests target in Makefile.

## JSON format for test suite results:
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

## Running tests:
`make build run_all_tests`

## Running a subset of tests:
`make build run_core_vm_tests`

