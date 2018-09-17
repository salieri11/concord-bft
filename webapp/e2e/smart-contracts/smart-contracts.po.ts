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
    element(by.css('#smartContractFrom')).sendKeys(from);
    element(by.css('#smartContractContractId')).sendKeys(contractId);
    element(by.css('#smartContractVersion')).sendKeys(version);
    element(by.css('#smartContractFile')).sendKeys(filePath);
    element(by.css('button#contract_form_submit')).click();
  }

  getTableLinkElement(text) {
    return element(by.cssContainingText('.datagrid-cell a', text));
  }
}
