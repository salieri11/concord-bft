/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';

export class SmartContractsPage {
  navigateTo() {
    return browser.get('/smart-contracts');
  }

  openCreateModal() {
    element(by.css('.create-contract')).click();
  }

  fillContractForm(from, contractId, version, filePath) {
    element(by.css('#smart-contract-from')).sendKeys(from);
    element(by.css('#smart-contract-contract-id')).sendKeys(contractId);
    element(by.css('#smart-contract-version')).sendKeys(version);
    element(by.css('#smart-contract-file')).sendKeys(filePath);
    element(by.css('.contract-form button[type="submit"]')).click();
  }

  getTableLinkElement(text) {
    return element(by.cssContainingText('.datagrid-cell a', text));
  }
}
