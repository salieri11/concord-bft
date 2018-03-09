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

## Running a test suite:
`./main.py CoreVMTests`
