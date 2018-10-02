/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { browser } from 'protractor';

import { AuthHelper } from '../helpers/auth';
import { AppPage } from '../app/app.po';
import { LoginPage } from '../login/login.po';
import { OnboardingPage } from './onboarding.po';
import { DashboardPage } from '../dashboard/dashboard.po';

describe('athena-ui Onboarding Flow', () => {
  let authHelper: AuthHelper;
  let appPage: AppPage;
  let dashboardPage: DashboardPage;
  let onboardingPage: OnboardingPage;
  let loginPage: LoginPage;

  afterEach(() => {
    authHelper = new AuthHelper();
    authHelper.deleteSessionData();
  });

  beforeEach(() => {
    appPage = new AppPage();
    dashboardPage = new DashboardPage();
    onboardingPage = new OnboardingPage();
    loginPage = new LoginPage();
    onboardingPage.navigateTo();
  });

  it('should onboard to the org tour', () => {
    browser.sleep(200);
    onboardingPage.readAndClickAccept('Test', 'Test', 'Company');
    loginPage.fillLogInForm('testlogin@example.com', 'password');
    loginPage.changePassword('T3sting!', 'T3sting!!');
    expect(loginPage.getChangeSubmit().getAttribute('disabled')).toBe('true');
    loginPage.changePassword('T3sting!', 'T3sting!');
    browser.sleep(200);
    loginPage.changePasswordSubmit();
    browser.sleep(200);
    browser.waitForAngularEnabled(false);

    expect(appPage.getTourTitle().getText()).toEqual('Node Status');
    appPage.getTourNextButton().click();
    browser.sleep(200);
    expect(appPage.getTourTitle().getText()).toEqual('Transaction List');
    appPage.getTourNextButton().click();
    browser.sleep(200);
    expect(appPage.getTourTitle().getText()).toEqual('Manage Smart contracts');
    appPage.getTourNextButton().click();
    browser.sleep(200);
    expect(appPage.getTourTitle().getText()).toEqual('Create');
    appPage.getTourNextButton().click();
    browser.sleep(200);
    expect(appPage.getTourTitle().getText()).toEqual('Manage users');
    appPage.getTourNextButton().click();
    browser.sleep(200);
    expect(appPage.getTourTitle().getText()).toEqual('User Actions');
    appPage.getTourNextButton().click();
    browser.sleep(200);
    expect(appPage.getTourTitle().getText()).toEqual('User settings');
    appPage.getTourNextButton().click();
    browser.sleep(200);
    expect(appPage.getTourNextButton().isDisplayed()).toBe(false);
    expect(appPage.getTourTitle().getText()).toEqual('Download certificate');
    appPage.clickTourEndButton();
    browser.sleep(200);
    expect(appPage.getTourTitle().isDisplayed()).toBe(false);
    browser.waitForAngularEnabled(true);
  });

  it('should onboard to deploy blockchain', () => {
    browser.sleep(200);
    loginPage.fillLogInForm('testlogin@example.com', 'T3sting!');

    browser.waitForAngularEnabled(false);
    browser.sleep(200);
    dashboardPage.navigateToDeploy();
    browser.sleep(200);

    expect(dashboardPage.getBlockchainWizardForm().isPresent()).toBe(true);
    // First page
    expect(dashboardPage.getHeaderWithText('Setup Blockchain and Consortium').isPresent()).toBe(true);
    dashboardPage.fillPage1Form('athena', 'global', 'Test Consortium');
    dashboardPage.clickWizardNextButton();
    // Second page
    expect(dashboardPage.getHeaderWithText('Add Organizations').isPresent()).toBe(true);
    dashboardPage.fillPage2Form('Test Org', 'sydney');
    expect(dashboardPage.getOrgsTableCellWithText('Test Org').isPresent()).toBe(true);
    dashboardPage.clickWizardNextButton();
    // Third page
    expect(dashboardPage.getHeaderWithText('Users & Access').isPresent()).toBe(true);
    dashboardPage.fillPage3Form('Test', 'User', 'test@vmware.com', 'Test Org', 'system_admin');
    expect(dashboardPage.getUsersTableCellWithText('test@vmware.com').isPresent()).toBe(true);
    dashboardPage.clickWizardNextButton();
    // Fourth Page
    expect(dashboardPage.getHeaderWithText('Review Deployment Setup').isPresent()).toBe(true);
    expect(dashboardPage.getReviewRowWithText('Athena').isPresent()).toBe(true);
    expect(dashboardPage.getReviewRowWithText('Test Consortium').isPresent()).toBe(true);
    expect(dashboardPage.getOrgsTableCellWithText('Test Org').isPresent()).toBe(true);
    expect(dashboardPage.getUsersTableCellWithText('test@vmware.com').isPresent()).toBe(true);

    dashboardPage.clickAdvancedAccordionHeader();
    expect(dashboardPage.getAdvancedNetworkNameValue()).toEqual('Test Consortium Net');
    expect(dashboardPage.getAdvancedNumberOfNodesValue()).toEqual('36');
    expect(dashboardPage.getAdvancedPublicNodesRegionsValueLength()).toEqual(17);
    expect(dashboardPage.getAdvancedPrivateNodeValue()).toEqual('');

    dashboardPage.clickWizardFinishButton();

    expect(dashboardPage.getTasks().count()).toBe(5);

    browser.waitForAngularEnabled(true);
  });
});
