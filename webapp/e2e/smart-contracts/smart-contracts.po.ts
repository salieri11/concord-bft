/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';

export class SmartContractsPage {
  navigateTo() {
    return browser.get('/smart-contracts');
  }

  openCreateModal() {
    browser.waitForAngularEnabled(false);
    element(by.css('.create-contract')).click();
  }

  fillContractFormStep1(from, contractId, version, filePath) {
    element(by.css('#smartContractFrom')).sendKeys(from);
    element(by.css('#smartContractContractId')).sendKeys(contractId);
    element(by.css('#smartContractVersion')).sendKeys(version);
    element(by.css('#smartContractFile')).sendKeys(filePath);
  }

  getTableLinkElement(text) {
    return element(by.cssContainingText('.datagrid-cell a', text));
  }

  clickWizardNextButton() {
    element(by.cssContainingText('button.clr-wizard-btn--primary', 'Next')).click();
  }

  clickWizardFinishButton() {
    element(by.cssContainingText('button.clr-wizard-btn--primary.btn-success', 'Finish')).click();
  }
}
