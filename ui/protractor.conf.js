// Protractor configuration file, see link for more information
// https://github.com/angular/protractor/blob/master/lib/config.ts

const { SpecReporter } = require('jasmine-spec-reporter');
const Logger = require('protractor/built/logger').Logger,
  logger = new Logger('JasmineFailLogger');

exports.config = {
  allScriptsTimeout: 31000,
  specs: [
    './e2e/onboarding/onboarding.e2e-spec.ts',
    './e2e/smart-contracts/smart-contracts.e2e-spec.ts',
    './e2e/swagger/swagger.e2e-spec.ts',
  ],
  capabilities: {
    'browserName': 'chrome'
  },
  directConnect: true,
  baseUrl: 'https://localhost.vmware.com/',
  framework: 'jasmine',
  jasmineNodeOpts: {
    showColors: true,
    defaultTimeoutInterval: 30000,
    print: function() {}
  },
  onPrepare() {
    require('ts-node').register({
      // Try using absolute path of e2e/tsconfig.e2e.json if not found.
      project: 'e2e/tsconfig.e2e.json'
    });
    jasmine.getEnv().addReporter(new SpecReporter({ spec: { displayStacktrace: true } }));
    prepareJasmineForFlake();
    browser.ignoreSynchronization = true;
  }
};

prepareJasmineForFlake = function () {
// Checking Jasmine.prototype.loadSpecs.toString() breaks e2e, comment out

//   let expectedJasmineLoadSpec =
//     `function () {
//   this.specFiles.forEach(function(file) {
//     require(file);
//   });
// }`;

  let currSpecFile;

  const Jasmine = require('jasmine/lib/jasmine');

  // Checking Jasmine.prototype.loadSpecs.toString() breaks e2e, comment out

  // if (Jasmine.prototype.loadSpecs.toString() !== expectedJasmineLoadSpec) {
  //   logger.info(Jasmine.prototype.loadSpecs.toString());
  //   throw new Error(`Jasmine.prototype.loadSpecs is not as expected, refusing to modify it`);
  // }

  Jasmine.prototype.loadSpecs = function () {
    this.specFiles.forEach(function (file) {
      currSpecFile = file;
      try {
        require(file);
      } finally {
        currSpecFile = null;
      }
    });
  };

  // A mapping of spec IDs to the path of the spec file they are defined at
  // More precisely this is the files that requiring it caused the spec to be created
  const specFiles = {};

  // Override jasmine's global it function to connect each spec ID with a file path
  const originalIt = global.it;
  if (!originalIt) throw new Error(`global.it is not defined. Can't mock it now`);
  global.it = function () {
    const spec = originalIt.apply(this, arguments);
    specFiles[spec.id] = currSpecFile;
    return spec;
  };

  class JasmineFailuresReporter {
    specDone(result) {
      if (result.status === 'failed') {
        const failedSpecFile = specFiles[result.id];

        if (!failedSpecFile) {
          // We might be here due to a declaration exception in the specs and these messages
          // will help us understand why we have it
          logger.error(`Spec ${result.id} failed for the following reasons and we can't tell where it was defined`);
          result.failedExpectations.forEach((expectation) => {
            logger.error(expectation.message);
            logger.error(expectation.stack);
          });

          throw new Error(`Can't tell spec file for failed spec ${result.id} ${result.fullName}`);
        }
        logger.info(`A Jasmine error occured at UserContext.it (${failedSpecFile}:)`);
      }
    }
  }

  jasmine.getEnv().addReporter(new JasmineFailuresReporter());
};
