/*
 * Copyright 2019 VMware, all rights reserved.
 */

const MEMORY_POINTER_4_22 = '6080604052';
const MEMORY_POINTER_4_7 = '6060604052';
const SWARM_HASH_START = 'a165627a7a72305820';

function parseByteCode(bytecode, solidityVersion) {
  const solcMinor = parseMinorVersion(solidityVersion);
  const solcPatch = parsePatchVersion(solidityVersion);

  if ((solcMinor >= 4 && solcPatch >= 22) || solcMinor >= 5) {
    const startingPoint = bytecode.lastIndexOf(MEMORY_POINTER_4_22);
    const endingPoint = bytecode.search(SWARM_HASH_START);
    return bytecode.slice(startingPoint, endingPoint);
  } else if (solcMinor >= 4 && solcPatch >= 7) {
    const startingPoint = bytecode.lastIndexOf(MEMORY_POINTER_4_7);
    const endingPoint = bytecode.search(SWARM_HASH_START);
    return bytecode.slice(startingPoint, endingPoint);
  } else {
    return bytecode;
  }
}

function parseMinorVersion(version) {
  return parseInt(version.match(/v\d+?\.\d+?\.\d+?[+-]/gi)[0].match(/\.\d+/g)[0].slice(1), 10);
}

function parsePatchVersion(version) {
  return parseInt(version.match(/v\d+?\.\d+?\.\d+?[+-]/gi)[0].match(/\.\d+/g)[1].slice(1), 10);
}

module.exports = {
  parseByteCode,
  parseMinorVersion,
  parsePatchVersion
};
