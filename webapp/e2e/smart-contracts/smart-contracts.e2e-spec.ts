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
  let valueString: string;
  let file: string;

  beforeAll(() => {
    loginPage = new LoginPage();
    authHelper = new AuthHelper();
    loginPage.navigateTo();
    loginPage.fillLogInForm('testlogin@example.com', 'password');
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
    valueString = '0x10';
    file = '../files/somefile.sol';
    smartContractsPage.navigateTo();
  });

  it('should create a smart contract', () => {
    const absolutePath = path.resolve(__dirname, file);
    const expectedLinkText = `${contractId} : ${from}`;
    smartContractsPage.openCreateModal();
    smartContractsPage.fillContractForm(from, contractId, version, absolutePath);

    expect(smartContractsPage.getTableLinkElement(expectedLinkText).isPresent()).toBe(true);
  });

  it('should navigate to the smart contract page and allow version selection', () => {
    const expectedLinkText = `${contractId} : ${from}`;

    smartContractsPage.getTableLinkElement(expectedLinkText).click();

    expect(smartContractPage.getContractId()).toBe(contractId);
    expect(smartContractPage.getFunctionsForm().isPresent()).toBe(false);

    smartContractPage.chooseVersion(version);

    expect(smartContractPage.getVersionName()).toBe(version);
    expect(smartContractPage.getFunctionsForm().isPresent()).toBe(true);
  });

  it('should send a call when call is clicked with a valid form', () => {
    smartContractPage.navigateTo(contractId, version);
    expect(smartContractPage.getCallSuccessAlert().isPresent()).toBe(false);

    smartContractPage.fillParameterForm(valueString, '', from, 'call');

    expect(smartContractPage.getCallSuccessAlert().isPresent()).toBe(true);
  });

  it('should send a transaction when transaction is clicked with a valid form', () => {
    smartContractPage.navigateTo(contractId, version);
    expect(smartContractPage.getTransactionSuccessAlert().isPresent()).toBe(false);

    smartContractPage.fillParameterForm(valueString, valueString, from, 'transaction');

    expect(smartContractPage.getTransactionSuccessAlert().isPresent()).toBe(true);
  });
});
