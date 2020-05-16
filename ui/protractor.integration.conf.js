// Protractor configuration file, see link for more information
// https://github.com/angular/protractor/blob/master/lib/config.ts

const { SpecReporter } = require('jasmine-spec-reporter');
const Logger = require('protractor/built/logger').Logger,
  logger = new Logger('JasmineFailLogger');
const HtmlScreenshotReporter = require('protractor-jasmine2-screenshot-reporter');
const fs = require('fs');

// See BC-2712 for more information
const credentials = JSON.parse(fs.readFileSync('e2e/credentials.json', 'utf8'));
if (credentials.status === '<CREDENTIALS_NOT_INJECTED_FROM_JENKINS>') {
  console.error('Proper credentials are not injected to begin testing. '
                + 'Please properly set "ui/e2e/credentials.json"');
  process.exit(1);
}

function getFolderString() { // generates date and time in this format: 'd-m-yyyy_HH-MM-SS'
  const d = new Date();
  const datestamp = [d.getDate(), d.getMonth() + 1, d.getFullYear()].join('-') + '_' + [d.getHours(), d.getMinutes(), d.getSeconds()].join('-');
  let prePath = 'build';

  try {
    // Get path from file created by Hermes
    prePath = fs.readFileSync('ui_e2e_path.txt', 'utf8');
  } catch(e) {
    console.warn('Using default path, screenshot path from file not found', e);
  }
  return `${prePath}/screenshots_${datestamp}`;
}

const screenshotReporter = new HtmlScreenshotReporter({
  dest: getFolderString(),
  filename: 'screenshots-report.html',
  reportFailedUrl: true,
  reportOnlyFailedSpecs: false,
  captureOnlyFailedSpecs: false
});

exports.config = {
  allScriptsTimeout: 8 * 60 * 1000, // 8 minutes, needed for deploying.
  specs: [
    './e2e/login/login.e2e-spec.ts',
    './e2e/zone/zone.e2e-spec.ts',
    './e2e/deploy/deploy.e2e-spec.ts'
  ],
  capabilities: {
    browserName: 'chrome',
    trustAllSSLCertificates: true,
    acceptInsecureCerts: true,
    ACCEPT_SSL_CERTS: true,
    chromeOptions: {
      args: ['incognito'] // Avoids ERR_CACHE_MISS
    }
  },
  params: {
    credentials: credentials,
  },
  directConnect: true,
  baseUrl: 'https://localhost.vmware.com/',
  framework: 'jasmine',
  jasmineNodeOpts: {
    showColors: true,
    defaultTimeoutInterval: 8 * 60 * 1000, // 8 minutes, needed for deploying.
    print: function() { }
  },
  beforeLaunch: function() {
    return new Promise(function(resolve) {
      screenshotReporter.beforeLaunch(resolve);
    });
  },

  onPrepare() {
    require('ts-node').register({
      // Try using absolute path of `e2e/tsconfig.e2e.json` if node fails to find this in your local env.
      project: 'e2e/tsconfig.e2e.json'
    });
    jasmine.getEnv().addReporter(new SpecReporter({ spec: { displayStacktrace: true } }));
    jasmine.getEnv().addReporter(screenshotReporter);
    prepareJasmineForFlake();
    browser.ignoreSynchronization = true;
    browser.driver.manage().window().setSize(1600, 2500);
  }
};

prepareJasmineForFlake = function() {
  /* warning: `function ()` !== `function()`, watch out for spaces;
   * some node versions seem to toStrings function bodies differently!
  */
  let expectedJasmineLoadSpec =
    `function () {
  this.specFiles.forEach(function(file) {
    require(file);
  });
}`.replace(/ /g, ''); // Remove all spaces, for better comparison free of `toString` idiosyncrasies.

  let currSpecFile;

  const Jasmine = require('jasmine/lib/jasmine');

  const loadSpecsString = Jasmine.prototype.loadSpecs.toString().replace(/ /g, ''); // Remove all spaces also
  if (loadSpecsString !== expectedJasmineLoadSpec) {
    logger.info(Jasmine.prototype.loadSpecs.toString());
    throw new Error(`Jasmine.prototype.loadSpecs is not as expected, refusing to modify it`);
  }

  Jasmine.prototype.loadSpecs = function() {
    this.specFiles.forEach(function(file) {
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
  global.it = function() {
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
