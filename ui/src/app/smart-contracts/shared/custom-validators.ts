/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { AbstractControl, ValidatorFn } from '@angular/forms';

import * as Web3Utils from 'web3-utils';

const solidityNumberRange = {
  'signedInt': {
    'min': {
      '8': '-0x80',
      '16': '-0x8000',
      '24': '-0x800000',
      '32': '-0x80000000',
      '40': '-0x8000000000',
      '48': '-0x800000000000',
      '56': '-0x80000000000000',
      '64': '-0x8000000000000000',
      '72': '-0x800000000000000000',
      '80': '-0x80000000000000000000',
      '88': '-0x8000000000000000000000',
      '96': '-0x800000000000000000000000',
      '104': '-0x80000000000000000000000000',
      '112': '-0x8000000000000000000000000000',
      '120': '-0x800000000000000000000000000000',
      '128': '-0x80000000000000000000000000000000',
      '136': '-0x8000000000000000000000000000000000',
      '144': '-0x800000000000000000000000000000000000',
      '152': '-0x80000000000000000000000000000000000000',
      '160': '-0x8000000000000000000000000000000000000000',
      '168': '-0x800000000000000000000000000000000000000000',
      '176': '-0x80000000000000000000000000000000000000000000',
      '184': '-0x8000000000000000000000000000000000000000000000',
      '192': '-0x800000000000000000000000000000000000000000000000',
      '200': '-0x80000000000000000000000000000000000000000000000000',
      '208': '-0x8000000000000000000000000000000000000000000000000000',
      '216': '-0x800000000000000000000000000000000000000000000000000000',
      '224': '-0x80000000000000000000000000000000000000000000000000000000',
      '232': '-0x8000000000000000000000000000000000000000000000000000000000',
      '240': '-0x800000000000000000000000000000000000000000000000000000000000',
      '248': '-0x80000000000000000000000000000000000000000000000000000000000000',
      '256': '-0x8000000000000000000000000000000000000000000000000000000000000000'
    },
    'max': {
      '8': '0x7f',
      '16': '0x7fff',
      '24': '0x7fffff',
      '32': '0x7fffffff',
      '40': '0x7fffffffff',
      '48': '0x7fffffffffff',
      '56': '0x7fffffffffffff',
      '64': '0x7fffffffffffffff',
      '72': '0x7fffffffffffffffff',
      '80': '0x7fffffffffffffffffff',
      '88': '0x7fffffffffffffffffffff',
      '96': '0x7fffffffffffffffffffffff',
      '104': '0x7fffffffffffffffffffffffff',
      '112': '0x7fffffffffffffffffffffffffff',
      '120': '0x7fffffffffffffffffffffffffffff',
      '128': '0x7fffffffffffffffffffffffffffffff',
      '136': '0x7fffffffffffffffffffffffffffffffff',
      '144': '0x7fffffffffffffffffffffffffffffffffff',
      '152': '0x7fffffffffffffffffffffffffffffffffffff',
      '160': '0x7fffffffffffffffffffffffffffffffffffffff',
      '168': '0x7fffffffffffffffffffffffffffffffffffffffff',
      '176': '0x7fffffffffffffffffffffffffffffffffffffffffff',
      '184': '0x7fffffffffffffffffffffffffffffffffffffffffffff',
      '192': '0x7fffffffffffffffffffffffffffffffffffffffffffffff',
      '200': '0x7fffffffffffffffffffffffffffffffffffffffffffffffff',
      '208': '0x7fffffffffffffffffffffffffffffffffffffffffffffffffff',
      '216': '0x7fffffffffffffffffffffffffffffffffffffffffffffffffffff',
      '224': '0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffff',
      '232': '0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffff',
      '240': '0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffff',
      '248': '0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff',
      '256': '0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
    }
  },
  'unsignedInt': {
    'min': '0',
    'max': {
      '8': '0xff',
      '16': '0xffff',
      '24': '0xffffff',
      '32': '0xffffffff',
      '40': '0xffffffffff',
      '48': '0xffffffffffff',
      '56': '0xffffffffffffff',
      '64': '0xffffffffffffffff',
      '72': '0xffffffffffffffffff',
      '80': '0xffffffffffffffffffff',
      '88': '0xffffffffffffffffffffff',
      '96': '0xffffffffffffffffffffffff',
      '104': '0xffffffffffffffffffffffffff',
      '112': '0xffffffffffffffffffffffffffff',
      '120': '0xffffffffffffffffffffffffffffff',
      '128': '0xffffffffffffffffffffffffffffffff',
      '136': '0xffffffffffffffffffffffffffffffffff',
      '144': '0xffffffffffffffffffffffffffffffffffff',
      '152': '0xffffffffffffffffffffffffffffffffffffff',
      '160': '0xffffffffffffffffffffffffffffffffffffffff',
      '168': '0xffffffffffffffffffffffffffffffffffffffffff',
      '176': '0xffffffffffffffffffffffffffffffffffffffffffff',
      '184': '0xffffffffffffffffffffffffffffffffffffffffffffff',
      '192': '0xffffffffffffffffffffffffffffffffffffffffffffffff',
      '200': '0xffffffffffffffffffffffffffffffffffffffffffffffffff',
      '208': '0xffffffffffffffffffffffffffffffffffffffffffffffffffff',
      '216': '0xffffffffffffffffffffffffffffffffffffffffffffffffffffff',
      '224': '0xffffffffffffffffffffffffffffffffffffffffffffffffffffffff',
      '232': '0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffff',
      '240': '0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff',
      '248': '0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff',
      '256': '0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
    }
  }
};


export function isHexAddress(control: AbstractControl): { [key: string]: boolean } | null {

  if (control.value !== undefined && control.value !== null && !Web3Utils.isAddress(control.value)) {
    return {'hexAddress': true};
  }
  return null;
}

export function isHexadecimal(control: AbstractControl): { [key: string]: boolean } | null {
  if (control.value !== undefined && control.value !== null && !Web3Utils.isHex(control.value)) {
    return {'hexadecimal': true};
  }
  return null;
}

export function isUint(controlType: string): ValidatorFn {
  return (control: AbstractControl): { [key: string]: boolean } | null => {
    if (control.value !== undefined && control.value !== null) {
      try {
        const controlValueAsBigNumber = Web3Utils.toBN(control.value);
        const solidityRange = getBigNumberRange(controlType);
        if (!(controlValueAsBigNumber.lte(solidityRange.max) && controlValueAsBigNumber.gte(solidityRange.min))) {
          return {'unsignedInteger': true};
        }
      } catch (err) {
        return {'unsignedInteger': true};
      }
    }
    return null;
  };
}

export function isInt(controlType: string): ValidatorFn {
  return (control: AbstractControl): { [key: string]: boolean } | null => {
    if (control.value !== undefined && control.value !== null) {
      try {
        const controlValueAsBigNumber = Web3Utils.toBN(control.value);
        const solidityRange = getBigNumberRange(controlType);
        if (!(controlValueAsBigNumber.lte(solidityRange.max) && controlValueAsBigNumber.gte(solidityRange.min))) {
          return {'signedInteger': true};
        }
      } catch (err) {
        return {'signedInteger': true};
      }
    }
    return null;
  };
}

export function isBytes(controlType: string): ValidatorFn {
  const hexLength = getByteLength(controlType) * 2;
  return (control: AbstractControl): { [key: string]: boolean } | null => {
    if (control.value === undefined || control.value === null) {
      return null;
    }
    const controlValue = Web3Utils.isHexStrict(control.value) ? control.value : Web3Utils.asciiToHex(control.value);
    const controlValueHexLength = controlValue.length - 2;

    if (controlValueHexLength % 2 !== 0) {
      return {invalidByteLength: true};
    }
    if (controlType !== 'bytes' && controlValueHexLength > hexLength) {
      return {invalidByteLength: true};
    }
  };
}

export function isBytesArray(controlType: string): ValidatorFn {
  const bytesType = controlType.split('[')[0];
  const arrayLength = getArrayLength(controlType.split(bytesType)[1]);
  const hexLength = getByteLength(bytesType) * 2;

  return(control: AbstractControl): { [key: string]: boolean } | null => {
    if (control.value === undefined || control.value === null) {
      return null;
    }
    const controlArrayValue = control.value.split('\n');
    let hasIncorrectByteLength = false;
    // Check correct array length
    if (arrayLength !== -1 && controlArrayValue.length !== arrayLength) {
      return {invalidArrayLength: true};
    }
    controlArrayValue.forEach((item) => {
      // Check correct byte length in each item
      const controlValue = Web3Utils.isHexStrict(item) ? item : Web3Utils.asciiToHex(item);
      const controlValueHexLength = controlValue.length - 2;
      if (bytesType !== 'bytes' && controlValueHexLength > hexLength) {
        hasIncorrectByteLength = true;
      }
    });
    if (hasIncorrectByteLength) {
      return {invalidByteArrayByteLength: true};
    }
  };
}

export function newVersionValue(existingVersions: string[]): ValidatorFn {
  return (control: AbstractControl): any => {
    return (existingVersions.indexOf(control.value) !== -1) ? { versionExists: true } : null;
  };
}

function getBigNumberRange(controlType: string): any {
  let numberOfBits;
  let hexMinString;
  let hexMaxString;
  if (controlType === 'int' || controlType === 'uint') {
    numberOfBits = 256;
  } else {
    if (controlType.startsWith('u')) {
      numberOfBits = parseInt(controlType.substr(4), 10);
    } else {
      numberOfBits = parseInt(controlType.substr(3), 10);
    }
  }
  if (controlType.startsWith('u')) {
    hexMinString = solidityNumberRange.unsignedInt.min;
    hexMaxString = solidityNumberRange.unsignedInt.max[numberOfBits];
  } else {
    hexMinString = solidityNumberRange.signedInt.min[numberOfBits];
    hexMaxString = solidityNumberRange.signedInt.max[numberOfBits];
  }
  return {
    min: Web3Utils.toBN(hexMinString),
    max: Web3Utils.toBN(hexMaxString)
  };
}

function getByteLength(controlType) {
  if (controlType === 'byte') {
    return 1;
  } else if (controlType === 'bytes') {
    return -1;
  } else {
    return parseInt(controlType.split('bytes')[1], 10);
  }
}

function getArrayLength(arrayNotation) {
  if (arrayNotation === '[]') {
    return -1;
  } else {
    return parseInt(arrayNotation.replace(/[\[\]]/g, ''), 10);
  }
}

