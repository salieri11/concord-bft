/*

 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';

export class SmartContractsPage {

  navigateTo() {
    element(by.id('sidenav-smart-contracts')).click();
  }

  getPageTitle() {
    element(by.css('.page-title'));
  }

  openCreateModal() {
    element(by.css('.create-contract')).click();
  }

  fillContractFormStep1(from, contractId, version, compilerVersion, filePath) {
    element(by.css('#smartContractFrom')).sendKeys(from);
    element(by.css('#smartContractContractId')).sendKeys(contractId);
    element(by.css('#smartContractVersion')).sendKeys(version);
    element(by.css('#smartContractCompilerVersion')).sendKeys(compilerVersion);
    element(by.css('#smartContractFile')).sendKeys(filePath);
  }

  getTableLinkElement(text) {
    return element(by.cssContainingText('.datagrid-cell a', text));
  }

  clickWizardNextButton() {
    element(by.cssContainingText('button.clr-wizard-btn--primary', 'Next')).click();
  }

  addProprosals() {
    element(by.css('.clr-textarea-wrapper .clr-textarea')).sendKeys('fun\nno fun');
  }

  clickWizardFinishButton() {
    element(by.cssContainingText('button.clr-wizard-btn--primary.btn-success', 'Finish')).click();
  }
}
