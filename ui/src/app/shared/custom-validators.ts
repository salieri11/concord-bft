/*
 * Copyright 2018-2020 VMware, all rights reserved.
 */

import { AbstractControl, ValidatorFn } from '@angular/forms';
import { Subscription } from 'rxjs/internal/Subscription';

// tslint:disable-next-line:max-line-length
export const urlRegEx = /^(https?:\/\/)?((([a-z\d]([a-z\d-]*[a-z\d])*)\.)+[a-z]{2,}|((\d{1,3}\.){3}\d{1,3}))(\:\d+)?(\/[-a-z\d%_.~+]*)*(\?[;&a-z\d%_.~+=-]*)?(\#[-a-z\d_]*)?$/i;
// tslint:disable-next-line:max-line-length
export const ipRegEx = /^(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$/;
export const listOfIpsRegEx = new RegExp(['^((((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]',
  '|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.',
  '(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\,?)*)-?)*$'].join(''));


export function matchPasswordValidator(passwordFormControlName: string): ValidatorFn {
  return (control: AbstractControl): any => {
    const otherPasswordFormControl: AbstractControl = control.root.get(passwordFormControlName);

    if (otherPasswordFormControl) {
      const subscription: Subscription = otherPasswordFormControl.valueChanges.subscribe(() => {
        control.updateValueAndValidity();
        subscription.unsubscribe();
      });
    }
    return (otherPasswordFormControl && control.value !== otherPasswordFormControl.value) ? { mismatch: true } : null;
  };
}

export function validateNumberOfNodes(): ValidatorFn {
  return (control: AbstractControl): {[key: string]: any} | null => {
    const valid = (control.value - 1) % 3 === 0;
    return valid ? null : {'numberInvalid': {value: control.value}};
  };
}

export function protocolNotAllowed(options?: {optional?: boolean}): ValidatorFn {
  return (control: AbstractControl): {[key: string]: any} | null => {
    if (options && options.optional && !control.value) { return null; } // pass optional if falsey value (e.g empty string)
    const urlValid  = urlRegEx.test(control.value);
    const inValid = control.value.startsWith('http://') || control.value.startsWith('https://');

    return urlValid && !inValid ? null : {'protocolNotAllowed': {value: control.value}};
  };
}

const validCert = (control, header, footer): boolean => {
  if (!control.value) {
    return true
  }
  const cert = control.value.trim();
  const certArr = cert.split('\n');
  const validHeader: boolean = certArr[0] === header;
  const validFooter: boolean = certArr[certArr.length - 1] === footer;
  const validBody = (): boolean => {
    let valid = true;
    certArr.pop();
    certArr.shift();
    const body = certArr.join('\n');

    try {
      window.atob(body);
    } catch(_) {
      valid = false;
    }
    return valid;
  }
   return validHeader && validFooter && validBody();
}

export function validateCert(): ValidatorFn {
  return (control: AbstractControl): {[key: string]: any} | null => {
    console.log(control);
    return validCert(
      control,
      '-----BEGIN CERTIFICATE-----',
      '-----END CERTIFICATE-----'
    ) ? null : {'validCert': {value: control.value}};

  };
}

export function validatePEM(): ValidatorFn {
  return (control: AbstractControl): {[key: string]: any} | null => {

    return validCert(
      control,
      '-----BEGIN RSA PRIVATE KEY-----',
      '-----END RSA PRIVATE KEY-----'
    ) ? null : {'validCert': {value: control.value}};
  };
}
