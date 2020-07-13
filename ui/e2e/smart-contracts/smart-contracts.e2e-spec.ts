/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, protractor, ProtractorExpectedConditions, element, by } from 'protractor';

import { SmartContractsPage } from './smart-contracts.po';
import { SmartContractPage } from './smart-contract.po';
import { BROWSER_WAIT_TIME } from '../helpers/constants';
import { waitFor, waitForText, waitToDisappear } from '../helpers/utils';

declare var require: any;

const path = require('path');

describe('concord-ui Smart Contracts', () => {
  let smartContractsPage: SmartContractsPage;
  let smartContractPage: SmartContractPage;
  let from: string;
  let contractId: string;
  let version: string;
  let file: string;
  let until: ProtractorExpectedConditions;
  let compilerVersion: string;

  beforeAll(() => {
    until = protractor.ExpectedConditions;
    browser.waitForAngularEnabled(false);
  });

  beforeEach(() => {
    smartContractsPage = new SmartContractsPage();
    smartContractPage = new SmartContractPage();
    from = '0x5BB088F57365907B1840E45984CAE028A82AF934';
    contractId = 'contractId';
    version = 'version1';
    compilerVersion = '0.5.4';
    file = '../files/somefile.sol';
    browser.waitForAngularEnabled(false);
    waitFor('#sidenav-smart-contracts');
    smartContractsPage.navigateTo();
  });

  it('should create a smart contract', () => {
    const absolutePath = path.resolve(__dirname, file);
    smartContractsPage.openCreateModal();
    browser.wait(until.presenceOf(smartContractPage.getPageTitle()), BROWSER_WAIT_TIME);
    browser.sleep(300);
    smartContractsPage.fillContractFormStep1(from, contractId, version, compilerVersion, absolutePath);
    browser.sleep(300);
    smartContractsPage.clickWizardNextButton();
    waitForText(element(by.cssContainingText('.modal-title', 'Contract Selection')));
    smartContractsPage.clickWizardNextButton();
    waitForText(element(by.cssContainingText('.modal-title', 'Constructor Parameters')));
    smartContractsPage.addProprosals();
    smartContractsPage.clickWizardFinishButton();
    // waitFor('.contract-form');
    waitToDisappear('.clr-wizard');
    browser.sleep(500);
    expect(smartContractPage.getContractId()).toBe(contractId);
  });

  it('should navigate to the smart contract page with the latest version selected', () => {
    const expectedLinkText = `${contractId}`;
    waitFor('.datagrid-row a');
    smartContractsPage.getTableLinkElement(expectedLinkText).click();
    waitFor('.contract-form');
    browser.sleep(500);
    expect(smartContractPage.getContractId()).toBe(contractId);
    expect(smartContractPage.getVersionName()).toBe(version);
    expect(smartContractPage.getFunctionsForm().isPresent()).toBe(true);
  });

  it('should send a call when call is clicked with a valid form', () => {
    smartContractPage.navigateTo();
    waitFor('.contract-form');
    expect(smartContractPage.getCallSuccessAlert().isPresent()).toBe(false);
    browser.sleep(300);
    smartContractPage.fillParameterForm(from, 'call');
    browser.sleep(300);
    waitFor('.call-success');
    expect(smartContractPage.getCallSuccessAlert().isPresent()).toBe(true);
  });

  it('should send a transaction when transaction is clicked with a valid form', () => {
    smartContractPage.navigateTo();
    waitFor('.contract-form');
    expect(smartContractPage.getTransactionSuccessAlert().isPresent()).toBe(false);
    smartContractPage.fillParameterForm(from, 'transaction');
    waitFor('.send-success');
    const sendSuccessEl = smartContractPage.getTransactionSuccessAlert();
    const firstCallText = sendSuccessEl.getText();
    expect(sendSuccessEl.isPresent()).toBe(true);
    browser.sleep(500);
    smartContractPage.send();

    browser.sleep(1000);
    const secondCallText = sendSuccessEl.getText();
    expect(firstCallText).not.toBe(secondCallText);

  });
});
