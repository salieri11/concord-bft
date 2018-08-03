/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';

export class SmartContractPage {
  navigateTo(contractId, version) {
    let url = `/smart-contracts/${contractId}`;

    if (version) {
      url += `/versions/${version}`;
    }

    return browser.get(url);
  }

  getCallSuccessAlert() {
    return element(by.css('.alert-success'));
  }

  getTransactionSuccessAlert() {
    return element(by.css('.alert-success .alert-actions'));
  }

  getContractId() {
    return element(by.css('.page-title')).getText();
  }

  getVersionName() {
    return element(by.css('athena-smart-contract-version h2')).getText();
  }

  getFunctionsForm() {
    return element(by.css('div[formgroupname="contractForm"]'));
  }

  chooseVersion(version) {
    element(by.css('#selected-version')).sendKeys(version);
  }

  fillParameterForm(gas, value, from, type) {
    element(by.css('input[formcontrolname="gas"]')).sendKeys(gas);
    element(by.css('input[formcontrolname="value"]')).sendKeys(value);
    element(by.css('input[formcontrolname="from"]')).sendKeys(from);
    switch (type) {
      case 'call':
        element(by.css('#call-submit')).click();
        break;
      case 'transaction':
        element(by.css('#transaction-submit')).click();
        break;
      case 'preview':
        element(by.css('#preview-submit')).click();
        break;
    }
  }
}
