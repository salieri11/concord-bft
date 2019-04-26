/*
 * Copyright 2019 VMware, all rights reserved.
 */

const { Router } = require('express');
const solc = require('solc');

const { parseByteCode, parseMinorVersion, parsePatchVersion } = require('../helpers/validators');

const router = Router();
const tempFile = 'source.sol';
const fs = require('fs');

/* GET compile version json. */
router.get('/compiler_versions', compilerVersions);

/* POST compile a .sol file for a given compiler version and return contracts. */
router.post('/compile', compileWithVersion);

/* POST compile and verify a contract. */
router.post('/verify', verifyContractWithVersion);

module.exports = router;

function compilerVersions(req, res) {
  fs.readFile('public/compiler_versions.json', (err, content) => {
    if (err) throw err;
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(content, 'utf-8');
  });
}

function compileWithVersion(req, res, next) {
  // Inputs:
  // sourcecode
  // compilerVersion
  const { sourcecode, compilerVersion, isOptimize, runs } = req.body;

  solc.loadRemoteVersion(compilerVersion, (err, solcSnapshot) => {
    if (err) {
      next(err);
    } else {
      const input = generateInput(sourcecode, isOptimize, runs);

      try {
        const output = getOutput(solcSnapshot, input);

        if (outputIsValid(output)) {
          const response = buildCompileResponse(output, compilerVersion);

          res.json({
            data: response
          });
        } else {
          handleOutputError(output, res);
        }
      } catch (error) {
        console.error(error);
        next(new Error(`Unexpected error when compiling contract: ${error.message}`));
      }
    }
  });
}

function verifyContractWithVersion(req, res, next) {
  // Inputs:
  // sourcecode
  // compilerVersion
  // existingBytecode
  // selectedContract
  const {
    sourcecode, compilerVersion, isOptimize, runs, existingBytecode, selectedContract
  } = req.body;

  solc.loadRemoteVersion(compilerVersion, (err, solcSnapshot) => {
    if (err) {
      next(err);
    } else {
      const input = generateInput(sourcecode, isOptimize, runs);

      try {
        const output = getOutput(solcSnapshot, input);

        if (outputIsValid(output)) {
          const response = buildVerifyResponse(output, compilerVersion, selectedContract, existingBytecode);

          res.json({
            data: response
          });
        } else {
          handleOutputError(output, res);
        }
      } catch (error) {
        console.error(error);
        next(new Error(`Unexpected error when verfiying contract: ${error.message}`));
      }
    }
  });
}

function buildCompileResponse(output, compilerVersion) {
  const response = [];
  const fileName = outputIncludesFileName(compilerVersion) ? tempFile : '';
  Object.keys(output.contracts[fileName]).forEach((key) => {
    const abi = output.contracts[fileName][key].abi;
    response.push({
      contract_name: key,
      metadata: getMetaData(output.contracts[fileName][key].metadata, abi),
      bytecode: output.contracts[fileName][key].evm.bytecode.object
    });
  });

  return response;
}

function buildVerifyResponse(output, compilerVersion, selectedContract, existingBytecode) {
  const fileName = outputIncludesFileName(compilerVersion) ? tempFile : '';
  const parsedCompiledBytecode = parseByteCode(
    output.contracts[fileName][selectedContract].evm.bytecode.object,
    compilerVersion
  );
  const parsedExistingBytecode = parseByteCode(existingBytecode, compilerVersion);

  return {
    verified: parsedCompiledBytecode === parsedExistingBytecode
  };
}

function generateInput(sourceCode, isOptimize, runs) {
  return {
    language: 'Solidity',
    sources: {
      [tempFile]: {
        content: sourceCode
      }
    },
    settings: {
      optimizer: {
        enabled: isOptimize,
        runs: runs
      },
      outputSelection: {
        '*': {
          '*': ['metadata', 'evm.bytecode']
        }
      }
    }
  };
}

function getMetaData(metadata, abi) {
  // The metadata key may be empty or missing from the output depending on the compiler version and
  // contents of a contract. If this is the case, add the compiled abi to the metadata object for consistency
  if (metadata && metadata !== '') {
    return JSON.parse(metadata);
  } else {
    return {
      output: {
        abi
      }
    };
  }
}

function getOutput(solcSnapshot, input) {
  return JSON.parse(solcSnapshot.compile(JSON.stringify(input)));
}

function handleOutputError(output, res) {
  res.status(400);
  res.json({
    error: output.errors
  });
}

function outputIncludesFileName(compilerVersion) {
  // The output of compiler versions <= 0.4.8 uses an empty string as the file name. This function checks if the
  // version is one whose output will include the file name so we can appropriately access compiled contracts.
  const solcMinor = parseMinorVersion(compilerVersion);
  const solcPatch = parsePatchVersion(compilerVersion);

  return (solcMinor >= 4 && solcPatch >= 9) || solcMinor >= 5;
}

function outputIsValid(output) {
  return output.contracts && Object.keys(output.contracts).length !== 0;
}
