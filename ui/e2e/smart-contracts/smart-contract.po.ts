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
    return element(by.css('.call-success'));
  }

  getTransactionSuccessAlert() {
    return element(by.css('.send-success'));
  }

  getContractId() {
    return element(by.css('.page-title')).getText();
  }

  getVersionName() {
    return element(by.css('#selectedVersion option:checked')).getText();
  }

  getFunctionsForm() {
    return element(by.css('div[formgroupname="contractForm"]'));
  }

  fillParameterForm(from, type) {
    element(by.cssContainingText('option', 'chairperson')).click();
    element(by.css('input[formcontrolname="from"]')).sendKeys(from);
    switch (type) {
      case 'call':
        element(by.css('#callSubmit')).click();
        break;
      case 'transaction':
        element(by.css('#transactionSubmit')).click();
        break;
      case 'preview':
        element(by.css('#previewSubmit')).click();
        break;
    }
  }
}
