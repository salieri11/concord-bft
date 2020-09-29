# Copyright 2019-2020 VMware, Inc.  All rights reserved. -- VMware Confidential
# DAML util file to perform daml tests

from distutils.version import LooseVersion
from pathlib import Path
from tempfile import NamedTemporaryFile
import glob
import json
import os
import requests
import ssl
import stat
import subprocess
import sys
import time
import urllib
import util.auth
import util.json_helper

if 'hermes_util' in sys.modules.keys():
   import hermes_util.daml.upload_dar as darutil
   import hermes_util.helper as helper
   import hermes_util.hermes_logging as hermes_logging
else:
   from . import upload_dar as darutil
   from util import hermes_logging, helper

log = hermes_logging.getMainLogger()

# Force using VMware artifactory for every external maven request.
VMWARE_MAVEN_SETTINGS = "resources/vmware_mvn_settings.xml"

LEDGER_API_REPO_KEY = "daml_ledger_api_repo"
LEDGER_API_TAG_KEY = "daml_ledger_api_tag"
DEFAULT_DAML_TEST = "SemanticTests"
DAML_TESTS_WHITELIST = [
   "ActiveContractsServiceIT",
   "ClosedWorldIT",
   "CommandDeduplicationIT",
   "CommandServiceIT",
   "CommandSubmissionCompletionIT",
   # "ConfigManagementServiceIT", # Broken on current CI setup: more participants will try to run it but only the first one will be authorized; disabled until solution is found
   "ContractKeysIT",
   "DivulgenceIT",
   "HealthServiceIT",
   "IdentityIT",
   "LedgerConfigurationServiceIT",
   "PackageManagementServiceIT",
   "PackageServiceIT",
   "PartyManagementServiceIT",
   "SemanticTests",
   "TransactionServiceIT"
   "WitnessesIT",
   "WronglyTypedContractIdIT",
   # "LotsOfPartiesIT",    # Not a functional test
   # "TransactionScaleIT", # Not a functional test
]

error_msg = None

# Get with get_ledger_api_version()
_ledger_api_version = ""

# Get with download_ledger_api_test_tool()
_ledger_api_test_tool_path = ""
_ledger_api_dars = []

# File which contains mappings of DAML SDK to Spider version.
DA_SPIDER_MAPPING_FILE = "resources/da_spider_sdk_mappings.json"

DA_SPIDER_IMAGE = "digitalasset/spider-application"

def get_ledger_api_version(host, results_dir=None):
   '''
   Returns the daml ledger api version from the daml ledger api repo Hermes is given
   or, if host is given, from the ledger api running on that host.
   '''
   global _ledger_api_version

   if results_dir:
      saved_ledger_api_version_file = os.path.join(results_dir, '..',
                                                   'ledger_api_version.json')

      if os.path.exists(saved_ledger_api_version_file):
         with open(saved_ledger_api_version_file, "r") as fp:
            data = json.load(fp)
            _ledger_api_version = data["ledger_api_version"]
            log.debug(
               "Daml ledger api version from version file ({}): {}".format(
                  saved_ledger_api_version_file, _ledger_api_version))

   if not _ledger_api_version:
      log.debug("Getting ledger api version from '{}'".format(host))
      user_config = util.json_helper.readJsonFile(helper.CONFIG_USER_FILE)
      username = user_config["persephoneTests"]["provisioningService"]["concordNode"]["username"]
      password = user_config["persephoneTests"]["provisioningService"]["concordNode"]["password"]

      if host == "localhost":
         # We're running daml in docker images
         ledger_api_repo = helper.get_docker_env(LEDGER_API_REPO_KEY)
         ledger_api_tag = helper.get_docker_env(LEDGER_API_TAG_KEY)
         cmd = ["docker", "run", ledger_api_repo + ":" + ledger_api_tag, "--", "--version"]
         log.info("cmd: {}".format(cmd))
         log.info("This may require downloading the ledger api image; please wait.")
         proc = subprocess.run(cmd, capture_output=True)
         proc.check_returncode()
         _ledger_api_version = proc.stdout.decode("UTF-8").strip()
      else:
         # We're running daml on remote systems.
         cmd = "docker exec -it daml_ledger_api /doc/daml/entrypoint.sh --version"
         max_attempts = 3
         num_attempts = 0

         while num_attempts < max_attempts:
            num_attempts += 1
            output = helper.ssh_connect(host, username, password, cmd)

            if output and output.strip():
               if "Error" in output:
                  raise Exception("Could not query Ledger API server on {}.  Received '{}'".format(host, output.strip()))
               else:
                  _ledger_api_version = output.strip()

               break
            else:
               if num_attempts >= max_attempts:
                  raise Exception("Unable to get the ledger api version from host '{}'".format(host))
               else:
                  log.info("Unable to get the ledger api version.  Retrying.")

      # save ledger api version
      if results_dir:
         saved_ledger_api_version_file = os.path.join(results_dir, '..',
                                                      'ledger_api_version.json')
         if not os.path.exists(saved_ledger_api_version_file):
            ledger_api_version_dict = {"ledger_api_version": _ledger_api_version}
            log.debug("Writing ledger api version json with value: {}".format(
               ledger_api_version_dict))
            with open(saved_ledger_api_version_file, 'w') as fp:
               json.dump(ledger_api_version_dict, fp, indent=2)

   return _ledger_api_version


def get_ledger_api_test_tool_name(host, file_format=True, results_dir=None):
   '''
   Returns the ledger api test tool name.
   file_format indicates whether to return the name of the jar file.
   Otherwise, it is the name of the mvn artifact.
   '''
   name = "ledger-api-test-tool"

   if file_format:
      name += "-"
   else:
      name += ":"

   name += get_ledger_api_version(host, results_dir=results_dir)

   if file_format:
      name += ".jar"

   return name


def extract_dars(jar):
   '''
   Given the ledger api test tool jar path, extract the dars and return
   a list of the dar files which were extracted.
   '''
   dar_dir = os.path.join(os.getcwd(), "resources", "test_dars")
   os.makedirs(dar_dir, exist_ok=True)
   cmd = ["java", "-jar", jar, "-x"]
   log.debug("Extracting dar files...")
   completedProcess = subprocess.run(cmd, capture_output=True, cwd=dar_dir)
   log.debug(completedProcess.stdout)
   completedProcess.check_returncode()

   dars = []
   pattern = "*.dar"

   for d,_,_ in os.walk(dar_dir):
      dars.extend(glob.glob(os.path.join(d, pattern)))

   log.debug("Extracted dars: {}".format(dars))
   return dars


def download_ledger_api_test_tool(host, results_dir=None, verbose=True):
   '''
   Downloads the ledger api test tool jar and returns:
   - The path to the jar.
   - A list of the dar files extracted from the jar.
   '''
   global _ledger_api_test_tool_path
   global _ledger_api_dars

   if not  _ledger_api_test_tool_path:
      mvn_repo = os.path.join(os.getcwd(), "resources", "mvn_repo")

      cmd = ["mvn", "--settings", VMWARE_MAVEN_SETTINGS, "dependency:get",
             "-Dartifact=com.daml:" + get_ledger_api_test_tool_name(host, False,
                                                                    results_dir=results_dir),
             "-Dmaven.repo.local=" + mvn_repo]

      max_attempts = 3
      num_attempts = 0

      while num_attempts < max_attempts and \
            not _ledger_api_test_tool_path:
         num_attempts += 1
         if verbose: log.info("Downloading ledger api test tool, attempt {} of {}".format(num_attempts, max_attempts))
         result, output = helper.execute_ext_command(cmd, False)

         if result:
            jar_path = None
            pattern = get_ledger_api_test_tool_name(host, True, results_dir=results_dir)

            for d,_,_ in os.walk(mvn_repo):
              jar_paths = glob.glob(os.path.join(d, pattern))

              if jar_paths:
                break

            if jar_paths:
               _ledger_api_test_tool_path = jar_paths[0]
               _ledger_api_dars = extract_dars(_ledger_api_test_tool_path)
            else:
               raise Exception("Ledger API test tool retrieved, but not found in {}".format(mvn_repo))
         else:
            log.error("Error downloading ledger api test tool.")
            log.error("Command: {}".format(cmd))
            log.error("Output: {}".format(output))

            if num_attempts >= max_attempts:
               raise Exception("That was the last attempt.  Triggering a failure.")

   return _ledger_api_test_tool_path, _ledger_api_dars


def upload_test_tool_dars(host='localhost', port='6861', results_dir=None, verbose=True):
   '''
   Helper method to upload test tool dar files
   :param host: daml-ledger-api host IP
   :param port: daml-ledger-api service port
   '''
   dars = download_ledger_api_test_tool(host, results_dir=results_dir, verbose=verbose)[1]
   if verbose: log.info("Upload DAR files to host '{}' and port '{}'...".format(host, port))

   for test_dar in dars:
      dar_uploaded = False
      max_retry_attempts = 10
      sleep_time = 30

      for i in range(0, max_retry_attempts):
         if verbose: log.info("  {} (attempt {}/{})...".format(test_dar, i+1, max_retry_attempts))

         try:
            dar_uploaded = darutil.upload_dar(host=host, port=port, darfile=test_dar)

            # upload_dar can also return False w/o raising an exception.
            if not dar_uploaded:
               raise Exception("DAR upload failed for an unknown reason.")

            break
         except Exception as e:
            if i != max_retry_attempts-1:
               if verbose: log.info("Retrying in 30 seconds...")
               time.sleep(sleep_time)
            else:
               log.error("Failed to upload test DAR " + test_dar)
               raise


def get_list_of_tests(host, run_all_tests=False, verbose=True):
   '''
   Helper method to list all the tests from test tool.jar
   :param run_all_tests: False to run only default set  of tests.
    True, to include the complete list of tests
   :return: list of tests
   '''
   tests = []
   cmd_list_tests = ["java", "-jar", download_ledger_api_test_tool(host)[0], "--list"]
   status, output = helper.execute_ext_command(cmd_list_tests, True)

   if status:
      for item in output.splitlines():
         if item and item.strip():
            item = item.split("*")[0].strip()

            if item in DAML_TESTS_WHITELIST:
               if run_all_tests:
                  tests.append(item.split("*")[0].strip())
               else:
                  if DEFAULT_DAML_TEST in item:
                     tests.append(item.split("*")[0].strip())
   else:
      raise Exception("Failed to fetch list of DAML tests.  Output: '{}'".format(output))

   if len(tests) == 0:
      raise Exception("No whitelisted DAML tests fetched.  Output from getting tests: '{}'".format(output))
   else:
      if verbose: log.info("List of tests: {}".format(tests))

   return tests

def verify_ledger_api_test_tool(ledger_endpoints=[('localhost','6861')],
                                run_all_tests=False, results_dir=None, verbose=True):
   '''
   Helper method to perform sanity check with uploaded dar files
   :param ledger_endpoints: List of 2-tuples representing ledger endpoint (host, port)
   '''
   if verbose: log.info("Performing DAML sanity checks...")

   host = ledger_endpoints[0][0] # First ledger api host

   if results_dir:
      results_sub_dir = helper.create_results_sub_dir(results_dir, host)

   overall_test_status = None
   for test in get_list_of_tests(host, run_all_tests=run_all_tests, verbose=verbose):
      cmd = [
         "java", "-jar", download_ledger_api_test_tool(host)[0],
         "--include", test,
         "--timeout-scale-factor", "20",
         "--no-wait-for-parties",
         "--concurrent-test-runs", "1",
         " ".join(["{}:{}".format(t[0], t[1]) for t in ledger_endpoints])
      ]
      if verbose: log.info("")
      if verbose: log.info("#### Run test '{}'... ####".format(test))
      log.debug("Command: {}".format(cmd))

      status, output = helper.execute_ext_command(cmd, verbose=verbose, timeout=360)

      if results_dir:
         file_prefix = "pass" if status else "fail"
         log_file = os.path.join(results_sub_dir,
                                 "{}_{}.log".format(file_prefix, test))
         if verbose: log.info("**** test log file: {}".format(log_file))
         with open(log_file, "w") as fp:
            fp.write(output)

      if status:
         if verbose: log.info("Test '{}' passed.".format(test))
      else:
         if verbose: log.info("Test '{}' failed.".format(test))
         log.error(output)
         overall_test_status = False
         continue

   if overall_test_status is False:
      raise Exception("Overall DAML testsuite Status: Fail")


def get_spider_version(product_sdk_version, username, password):
   '''
   Given a DAML SDK version, return a Spider app version which is compatible with it.
   '''
   spider_version = get_known_spider_version_for_sdk(product_sdk_version)

   if spider_version:
      return spider_version
   else:
      # This is for docker system calls.
      cmd = ["docker", "login", "--username", username, "--password-stdin"]
      subprocess.run(cmd, input=password, text=True)

      # This is for the API call.
      token = util.auth.get_dockerhub_auth_token(username, password)
      next_url = None
      max_calls = 1 # Not sure if we'll ever need to increase.
      num_calls = 0
      found = False

      while (num_calls < max_calls and not found):
         num_calls += 1

         if next_url:
            url = next_url
         else:
            # We can fetch up to 100 at a time, but it will probably be within the first few.
            url = "https://hub.docker.com/v2/repositories/{}/tags/?page_size=100".format(DA_SPIDER_IMAGE)

         log.info("Fetching {} tags from DockerHub".format(DA_SPIDER_IMAGE))
         response = requests.get(url, headers={"Authorization": "JWT " + token})
         response_obj = json.loads(response.text)
         results = response_obj["results"]
         spider_version = find_spider_version_result(results, product_sdk_version)

         if spider_version:
            return spider_version
         else:
            next_url = response_obj["next"]

         if not next_url:
            break


def find_spider_version_result(results, product_sdk_version):
   '''
   Given results from the /tags DockerHub API call, find (and return) a
   Spider version which is acceptable for the DAML sdk version.
   As of June 2020, this means the highest Spider version whose docker
   image contains a DAML SDK version value which is LESS than or equal
   to the DAML SDK version that is in the product we are testing.
   '''
   # First, sort Spider versions.  We receive the top x from DockerHub, but they
   # may be out of order, and the most recent is most likely to be what we want.
   all_spider_versions = []

   for result in results:
      all_spider_versions.append(LooseVersion(result["name"]))

   all_spider_versions.sort(reverse=True)
   log.info("Spider versions retrieved for inspection:")
   for v in all_spider_versions:
      log.info(v)


   for spider_version in all_spider_versions:
      log.info("Looking for DAML SDK version {} in {}:{}".format(product_sdk_version, DA_SPIDER_IMAGE, spider_version))
      cmd = ["docker", "pull", "{}:{}".format(DA_SPIDER_IMAGE, spider_version)]
      num_attempts = 3

      while num_attempts > 0:
         success, _ = helper.execute_ext_command(cmd, True)

         if success:
            break
         elif num_attempts > 0:
            num_attempts -= 1
            log.debug("Error pulling from DockerHub.  Retrying.")
         else:
            raise Exception("Could not pull from DockerHub.  Aborting.")

      cmd = ["docker", "inspect", "-f", "'{{json .Config.Labels}}'",
             "{}:{}".format(DA_SPIDER_IMAGE, spider_version)]
      success, stdout = helper.execute_ext_command(cmd, True)

      if not success:
         raise Exception("Failed to inspect {}:{}".format(DA_SPIDER_IMAGE, spider_version))

      # The data we want is surrounded by single quotes.
      labels_obj = json.loads(stdout[1:-2])
      found_sdk_version = labels_obj["da.version.sdk"]

      msg = "Found DAML SDK version {} in Spider version {}...".format(found_sdk_version, spider_version)

      if LooseVersion(found_sdk_version) <= LooseVersion(product_sdk_version):
         log.info(msg + "and we will use it.")
         return str(spider_version)
      else:
         msg = msg + "and we cannot use it.\n"
         msg += "  Product DAML SDK version: {}\n".format(product_sdk_version)
         msg += "  Spider DAML SDK version: {}".format(found_sdk_version)
         log.info(msg)

   return None


def get_known_spider_version_for_sdk(sdk_version):
   '''
   We have some SDK-to-Spider version mappings in a json file.  See if
   we have a match, and return the Spider version if we do.
   '''
   spider_version = None
   known_mappings = get_sdk_spider_version_mappings()

   if known_mappings and sdk_version in known_mappings:
      spider_version = known_mappings[sdk_version]
      log.info("Found mapping of DAML SDK {} to Spider {} in the known Spider/SDK mappings file.".format(sdk_version, spider_version))

   return spider_version


def get_sdk_spider_version_mappings():
   return util.json_helper.readJsonFile(DA_SPIDER_MAPPING_FILE)


def download_spider_app(tag):
   cmd = ["docker", "pull", "digitalasset/spider-application:" + tag]
   num_attempts = 3

   while num_attempts > 0:
      success, _ = helper.execute_ext_command(cmd, True)

      if success:
         break
      elif num_attempts > 0:
         num_attempts -= 1
         log.debug("Error pulling from DockerHub.  Retrying.")
      else:
         raise Exception("Could not pull from DockerHub.  Aborting.")

   if not success:
      raise Exception("Failed to pull digitalasset/spider-application:{}".format(spider_version))


def install_daml_sdk(sdk_version=None):
   '''
   sdk_version: Version to install.  If None, installs the latest according to DA's web site.
   Returns a path to the DAML SDK executable.
   Call this first to get the version: daml_sdk_version = daml_helper.get_ledger_api_version(participant_ip)
   '''
   home = str(Path.home())
   daml_path = os.path.join(home, ".daml", "bin", "daml")

   if os.path.isfile(daml_path) and sdk_version:
      cmd = [daml_path, "version"]
      success, output = util.helper.execute_ext_command(cmd, timeout=10, verbose=True)

      if success and sdk_version in output:
         log.info("DAML SDK version {} is already installed at {}".format(sdk_version, daml_path))
         return daml_path

   ssl._create_default_https_context = ssl._create_unverified_context
   url = "https://get.daml.com/"
   response = urllib.request.urlopen(url)
   data = response.read()
   text = data.decode("utf-8")
   script_file = os.path.join(os.getcwd(), "get-daml.sh")

   log.info("script_file: {}".format(script_file))

   with open(script_file, "w") as f:
      f.write(text)

   stat_info = os.stat(script_file)
   os.chmod(script_file, stat_info.st_mode | stat.S_IEXEC)
   cmd = [script_file]
   msg = "Installing DAML SDK: "

   if sdk_version:
      cmd.append(sdk_version)
      msg += sdk_version
   else:
      msg += "Latest available"

   log.info(msg)
   success, output = util.helper.execute_ext_command(cmd, timeout=600, verbose=True)

   if not success:
      raise Exception("Unable to install the DAML SDK. Details: {}".format(output))

   if os.path.isfile(daml_path):
      return daml_path
   else:
      raise Exception("Unable to find daml installation at {}".format(daml_path))
