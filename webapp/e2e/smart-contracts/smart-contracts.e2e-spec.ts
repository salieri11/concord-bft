/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { browser } from 'protractor';

import { SmartContractsPage } from './smart-contracts.po';
import { SmartContractPage } from './smart-contract.po';
import { AuthHelper } from '../helpers/auth';
import { LoginPage } from '../login/login.po';

declare var require: any;

const path = require('path');

describe('athena-ui Smart Contracts', () => {
  let authHelper: AuthHelper;
  let loginPage: LoginPage;
  let smartContractsPage: SmartContractsPage;
  let smartContractPage: SmartContractPage;
  let from: string;
  let contractId: string;
  let version: string;
  let file: string;

  beforeAll(() => {
    loginPage = new LoginPage();
    authHelper = new AuthHelper();
    loginPage.navigateTo();
    loginPage.fillLogInForm('admin@blockchain.local', 'T3sting!');
    browser.sleep(1000);
  });

  afterAll(() => {
    authHelper.logOut();
  });

  beforeEach(() => {
    smartContractsPage = new SmartContractsPage();
    smartContractPage = new SmartContractPage();
    from = '0x5BB088F57365907B1840E45984CAE028A82AF934';
    contractId = 'contractId';
    version = 'version1';
    file = '../files/somefile.sol';
    smartContractsPage.navigateTo();
  });

  it('should create a smart contract', () => {
    const absolutePath = path.resolve(__dirname, file);
    browser.sleep(200);
    smartContractsPage.openCreateModal();
    browser.sleep(200);
    smartContractsPage.fillContractFormStep1(from, contractId, version, absolutePath);
    browser.sleep(200);
    smartContractsPage.clickWizardNextButton();
    browser.sleep(200);
    smartContractsPage.clickWizardNextButton();
    browser.sleep(200);
    smartContractsPage.clickWizardFinishButton();
    browser.sleep(500);
    expect(smartContractPage.getContractId()).toBe(contractId);
  });

  it('should navigate to the smart contract page with the latest version selected', () => {
    const expectedLinkText = `${contractId}`;
    browser.sleep(200);
    smartContractsPage.getTableLinkElement(expectedLinkText).click();
    browser.sleep(200);
    expect(smartContractPage.getContractId()).toBe(contractId);

    expect(smartContractPage.getVersionName()).toBe(version);

    expect(smartContractPage.getFunctionsForm().isPresent()).toBe(true);
  });

  it('should send a call when call is clicked with a valid form', () => {
    smartContractPage.navigateTo(contractId, version);
    expect(smartContractPage.getCallSuccessAlert().isPresent()).toBe(false);
    browser.sleep(200);
    smartContractPage.fillParameterForm(from, 'call');
    browser.sleep(200);

    expect(smartContractPage.getCallSuccessAlert().isPresent()).toBe(true);
  });

  it('should send a transaction when transaction is clicked with a valid form', () => {
    browser.sleep(200);
    smartContractPage.navigateTo(contractId, version);
    browser.sleep(200);
    expect(smartContractPage.getTransactionSuccessAlert().isPresent()).toBe(false);
    smartContractPage.fillParameterForm(from, 'transaction');
    browser.sleep(500);
    expect(smartContractPage.getTransactionSuccessAlert().isPresent()).toBe(true);
  });
});
