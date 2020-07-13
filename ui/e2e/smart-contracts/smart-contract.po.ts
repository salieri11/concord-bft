/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';
import { waitFor } from '../helpers/utils';

export class SmartContractPage {
  navigateTo() {
    element(by.id('sidenav-smart-contracts')).click();
    waitFor('.datagrid-row a');
    element(by.css('.datagrid-row a')).click();
  }

  getCallSuccessAlert() {
    return element(by.css('.call-success'));
  }

  getTransactionSuccessAlert() {
    return element(by.css('.send-success'));
  }

  getPageTitle() {
    return element(by.css('.page-title'));
  }

  getContractId() {
    return element(by.css('.page-title')).getText();
  }

  getVersionName() {
    return element(by.css('#selectedVersion')).getText();
  }

  getFunctionsForm() {
    return element(by.css('div[formgroupname="contractForm"]'));
  }

  send() {
    element(by.css('#transactionSubmit')).click();
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
