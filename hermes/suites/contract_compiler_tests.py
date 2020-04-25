#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Tests covering Helen's non-ethereum ReST API.
# (i.e. everything under /api/, excluding /api/concord/eth)
#########################################################################
import logging
import os
import traceback

from . import test_suite
from suites.case import describe, passed, failed, getStackInfo
from rest.request import Request

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

contractBytecode_0_5_2 = ('608060405234801561001057600080fd5b5061013980610020600039'
                          '6000f3fe608060405234801561001057600080fd5b50600436106100'
                          '48576000357c01000000000000000000000000000000000000000000'
                          '000000000000009004806319ff1d211461004d575b600080fd5b6100'
                          '556100d0565b60405180806020018281038252838181518152602001'
                          '91508051906020019080838360005b83811015610095578082015181'
                          '84015260208101905061007a565b50505050905090810190601f1680'
                          '156100c25780820380516001836020036101000a0319168152602001'
                          '91505b509250505060405180910390f35b6060604080519081016040'
                          '5280600d81526020017f48656c6c6f2c20576f726c64210000000000'
                          '000000000000000000000000000081525090509056fea165627a7a72'
                          '305820cb955c3e65632e4e81915917d6892eecbbf51d49f3ae2e1aa9'
                          '454017d2a584b60029')

contractBytecode_0_4_19 = ('6060604052341561000f57600080fd5b6101578061001e600039600'
                           '0f300606060405260043610610041576000357c0100000000000000'
                           '000000000000000000000000000000000000000000900463fffffff'
                           'f16806319ff1d2114610046575b600080fd5b341561005157600080'
                           'fd5b6100596100d4565b60405180806020018281038252838181518'
                           '15260200191508051906020019080838360005b8381101561009957'
                           '808201518184015260208101905061007e565b50505050905090810'
                           '190601f1680156100c65780820380516001836020036101000a0319'
                           '16815260200191505b509250505060405180910390f35b6100dc610'
                           '117565b6040805190810160405280600d81526020017f48656c6c6f'
                           '2c20576f726c6421000000000000000000000000000000000000008'
                           '15250905090565b6020604051908101604052806000815250905600'
                           'a165627a7a723058202bf622d6a79b6a1044c23deae4f1c3e7b6fdd'
                           '6df49a8b4a03065078e60cfd06b0029')

contractBytecode_0_4_0 = ('606060405260a18060106000396000f360606040526000357c010000'
                          '00000000000000000000000000000000000000000000000000009004'
                          '806360fe47b11460435780636d4ce63c14605d57603f565b6002565b'
                          '34600257605b60048080359060200190919050506082565b005b3460'
                          '0257606c60048050506090565b604051808281526020019150506040'
                          '5180910390f35b806000600050819055505b50565b60006000600050'
                          '549050609e565b9056')

class ContractCompilerTests(test_suite.TestSuite):
    _args = None
    _userConfig = None
    _ethereumMode = False
    _productMode = True
    _resultFile = None

    def __init__(self, passedArgs, product):
        super(ContractCompilerTests, self).__init__(passedArgs, product)

    def getName(self):
        return "ContractCompilerTests"

    def run(self):
        ''' Runs all of the tests. '''
        p = self.launchProduct()

        if self._ethereumMode:
            info = "ContractCompilerTests are not applicable to ethereumMode."
            log.warn(info)
            self.writeResult("All tests", None, info, getStackInfo())
            return self._resultFile

        tests = self._getTests()

        for (testName, testFun) in tests:
            testLogDir = os.path.join(self._testLogDir, testName)

            try:
                result, info, stackInfo = self._runRestTest(testName,
                                                 testFun,
                                                 testLogDir)
            except Exception as e:
                result = False
                info = str(e)
                stackInfo = getStackInfo(e)
                traceback.print_tb(e.__traceback__)
                log.error("Exception running test: '{}'".format(info))

            if info:
                info += "  "
            else:
                info = ""

            relativeLogDir = self.makeRelativeTestPath(testLogDir)
            info += "Log: <a href=\"{}\">{}</a>".format(relativeLogDir,
                                                        testLogDir)
            self.writeResult(testName, result, info, stackInfo)

        log.info("Tests are done.")
        return super().run()

    def _runRestTest(self, testName, testFun, testLogDir):
        log.info("Starting test '{}'".format(testName))
        request = Request(testLogDir,
                          testName,
                          self.contractCompilerApiBaseUrl,
                          self._userConfig)
        return testFun(request)

    def _getTests(self):
        return [("contractCompilation", self._test_contractCompilation), \
                ("contractCompilation040", self._test_contractCompilation040), \
                ("contractCompilationFailed", self._test_contractCompilationFailed), \
                ("contractVerification", self._test_contractVerification), \
                ("contractVerification040", self._test_contractVerification040), \
                ("contractVerification0419", self._test_contractVerification0419), \
                ("contractVerificationFailedMismatchedContract", self._test_contractVerificationFailedMismatchedContract), \
                ("contractVerificationFailedMismatchedCompiler", self._test_contractVerificationFailedMismatchedCompiler), \
                ("contractVerificationFailedToCompile", self._test_contractVerificationFailedToCompile), \
                ("contractCompilationFailedInvalidFile", self._test_contractCompilationFailedInvalidFile), \
                ("contractCompilationFailedInvalidCompilerVersion", self._test_contractCompilationFailedInvalidCompilerVersion), \
                ("contractVerificationFailedInvalidFile", self._test_contractVerificationFailedInvalidFile), \
                ("contractVerificationFailedInvalidBytecode", self._test_contractVerificationFailedInvalidBytecode), \
                ("contractVerificationFailedInvalidCompilerVersion", self._test_contractVerificationFailedInvalidCompilerVersion)
        ]

    def contract_compile_util_generic(self, request, sourceCode, compilerVersion):

        data = {}
        data["sourcecode"] = sourceCode
        data["compilerVersion"] = compilerVersion
        data["isOptimize"] = True
        data["runs"] = 200
        return request.compileContract(data)

    def contract_compile_util(self, request, compilerVersion, sourceCode):
        '''
        A helper method to upload simple hello world contract.
        '''
        return self.contract_compile_util_generic(request,sourceCode,compilerVersion)

    def compile_mock_contract(self, request, compilerVersion=None, sourceCode=None):
        if compilerVersion is None:
            compilerVersion = "v0.5.2+commit.1df8f40c"
        if sourceCode is None:
            contractFile = open("resources/contracts/HelloWorld.sol", 'r')
            sourceCode = contractFile.read()
        result = self.contract_compile_util(request, compilerVersion,
                                           sourceCode)
        if "data" in result:
            return result["data"]
        else:
            raise Exception("Contract compilation failed with error '{}'".format(result["error"]))

    def contract_verify_util_generic(self, request, sourceCode, compilerVersion, existingBytecode, selectedContract):

        data = {}
        data["sourcecode"] = sourceCode
        data["compilerVersion"] = compilerVersion
        data["existingBytecode"] = existingBytecode
        data["selectedContract"] = selectedContract
        return request.verifyContract(data)

    def contract_verify_util(self, request, compilerVersion, sourceCode, existingBytecode, selectedContract):
        '''
        A helper method to verify a simple contract.
        '''
        return self.contract_verify_util_generic(request,sourceCode,compilerVersion, existingBytecode, selectedContract)

    def verify_mock_contract(self, request, compilerVersion=None, selectedContract=None, sourceCode=None, existingBytecode=None):
        if compilerVersion is None:
            compilerVersion = "v0.5.2+commit.1df8f40c"
        if selectedContract is None:
            selectedContract = "HelloWorld"
        if sourceCode is None:
            contractFile = open("resources/contracts/HelloWorldMultiple.sol", 'r')
            sourceCode = contractFile.read()
        if existingBytecode is None:
            existingBytecode = contractBytecode_0_5_2
        result = self.contract_verify_util(request, compilerVersion,
                                            sourceCode, existingBytecode, selectedContract)
        if "data" in result:
            return result["data"]
        else:
            raise Exception("Contract verification failed with error '{}'".format(result["error"]))



    # Tests: expect one argument, a Request, and produce a 2-tuple
    # (bool success, string info)
    @describe()
    def _test_contractCompilation(self, request):
        result = self.compile_mock_contract(request)

        if "metadata" in result[0]:
            return passed()
        else:
            return failed("Contract failed to compile")

    @describe()
    def _test_contractCompilation040(self, request):
        # solc outputs are different for compiler versions <= 4.4 as well as <= 4.7.
        # Need to test compilation and response structure
        compilerVersion = "v0.4.0+commit.acd334c9"
        contractFile = open("resources/contracts/SimpleStorage-0.4.0.sol", 'r')
        sourceCode = contractFile.read()

        result = self.compile_mock_contract(request, compilerVersion, sourceCode)

        if result[0].get("metadata", {}).get("output", {}).get("abi", False):
            return passed()
        else:
            return failed("Contract failed to compile")

    @describe()
    def _test_contractCompilationFailed(self, request):
        compilerVersion = "v0.4.17+commit.bdeb9e52"

        try:
            self.compile_mock_contract(request, compilerVersion)
        except Exception as e:
            if "Source file requires different compiler version" in str(e):
                return passed()
            else:
                print(e)
                return failed("Unexpected error when compiling contract")
        else:
            return failed("Failure when compiling contract with invalid compiler version")

    @describe()
    def _test_contractVerification(self, request):
        result = self.verify_mock_contract(request)

        if result["verified"] == True:
            return passed()
        else:
            return failed("Bytecode failed to verify")

    @describe()
    def _test_contractVerification0419(self, request):
        # bytecode verification is different in compiler versions from 0.4.8 and 0.4.21.
        # Need to test a compiled file in this range
        compilerVersion = "v0.4.19+commit.c4cbbb05"
        result = self.verify_mock_contract(request, compilerVersion, None, None, contractBytecode_0_4_19)

        if result["verified"] == True:
            return passed()
        else:
            return failed("Bytecode failed to verify")

    @describe()
    def _test_contractVerification040(self, request):
        # bytecode verification is different in compiler versions < 0.4.7.
        # Need to test a compiled file in this range
        compilerVersion = "v0.4.0+commit.acd334c9"
        contractFile = open("resources/contracts/SimpleStorage-0.4.0.sol", 'r')
        sourceCode = contractFile.read()
        selectedContract = "SimpleStorage"
        result = self.verify_mock_contract(request, compilerVersion, selectedContract, sourceCode, contractBytecode_0_4_0)

        if result["verified"] == True:
            return passed()
        else:
            return failed("Bytecode failed to verify")

    @describe()
    def _test_contractVerificationFailedMismatchedContract(self, request):
        selectedContract = "DummyContract"
        result = self.verify_mock_contract(request, None, selectedContract)

        if result["verified"] == False:
            return passed()
        else:
            return failed("Bytecode verified with mismatched contract")

    @describe()
    def _test_contractVerificationFailedMismatchedCompiler(self, request):
        compilerVersion = "v0.4.19+commit.c4cbbb05"
        result = self.verify_mock_contract(request, compilerVersion)

        if result["verified"] == False:
            return passed()
        else:
            return failed("Bytecode verified with mismatched compiler version")

    @describe()
    def _test_contractVerificationFailedToCompile(self, request):
        compilerVersion = "v0.4.17+commit.bdeb9e52"

        try:
            self.verify_mock_contract(request, compilerVersion)
        except Exception as e:
            if "Source file requires different compiler version" in str(e):
                return passed()
            else:
                print(e)
                return failed("Unexpected error when verifying contract")
        else:
            return failed("Failure when validating contract with invalid compiler version")

    @describe()
    def _test_contractCompilationFailedInvalidFile(self, request):
        try:
            self.compile_mock_contract(request, None, "")
        except Exception as e:
            if "Source file does not specify required compiler version" in str(e):
                return passed()
            else:
                print(e)
                return failed("Unexpected error when compiling contract")
        else:
            return failed("Failure when compiling invalid source code")

    @describe()
    def _test_contractCompilationFailedInvalidCompilerVersion(self, request):
        try:
            self.compile_mock_contract(request, "test")
        except Exception as e:
            if "Error retrieving binary" in str(e):
                return passed()
            else:
                print(e)
                return failed("Unexpected error when compiling contract")
        else:
            return failed("Failure when compiling with invalid compiler version")

    @describe()
    def _test_contractVerificationFailedInvalidFile(self, request):
        try:
            self.verify_mock_contract(request, None, None, "")
        except Exception as e:
            if "Source file does not specify required compiler version" in str(e):
                return passed()
            else:
                print(e)
                return failed("Unexpected error when verifying contract")
        else:
            return failed("Failure when validating invalid source code")

    @describe()
    def _test_contractVerificationFailedInvalidBytecode(self, request):
        result = self.verify_mock_contract(request, None, None, None, "")

        if result["verified"] == False:
            return passed()
        else:
            return failed("Bytecode verified with invalid existing bytecode")

    @describe()
    def _test_contractVerificationFailedInvalidCompilerVersion(self, request):
        try:
            self.verify_mock_contract(request, "test")
        except Exception as e:
            if "Error retrieving binary" in str(e):
                return passed()
            else:
                print(e)
                return failed("Unexpected error when verifying contract")
        else:
            return failed("Failure when validating with invalid compiler version")
