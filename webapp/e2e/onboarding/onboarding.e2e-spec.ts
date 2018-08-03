/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { browser } from 'protractor';

import { AuthHelper } from '../helpers/auth';
import { MarketingPage } from '../marketing/marketing.po';
import { AppPage } from '../app/app.po';
import { SignUpPage } from '../sign-up/sign-up.po';
import { OnboardingPage } from './onboarding.po';
import { DashboardPage } from '../dashboard/dashboard.po';

describe('athena-ui Onboarding Flow', () => {
  let authHelper: AuthHelper;
  let appPage: AppPage;
  let dashboardPage: DashboardPage;
  let marketingPage: MarketingPage;
  let onboardingPage: OnboardingPage;
  let signUpPage: SignUpPage;

  afterEach(() => {
    authHelper = new AuthHelper();
    authHelper.deleteSessionData();
  });

  beforeEach(() => {
    appPage = new AppPage();
    dashboardPage = new DashboardPage();
    marketingPage = new MarketingPage();
    onboardingPage = new OnboardingPage();
    signUpPage = new SignUpPage();
    marketingPage.navigateTo();
  });

  it('should onboard to the org tour', () => {
    marketingPage.clickSignUpButton();

    expect(signUpPage.getSignUpForm().isPresent()).toBe(true);

    signUpPage.fillSignUpForm(
      'Test',
      'User',
      'test@vmware.com',
      'Test Company',
      'Test Engineer',
      'UNITED STATES',
      'Customer',
      '0-99'
    );

    onboardingPage.clickSetupOrg();

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
    marketingPage.clickSignUpButton();

    expect(signUpPage.getSignUpForm().isPresent()).toBe(true);

    signUpPage.fillSignUpForm(
      'Test',
      'User',
      'test@vmware.com',
      'Test Company',
      'Test Engineer',
      'UNITED STATES',
      'Customer',
      '0-99'
    );

    onboardingPage.clickDeployBlockchain();

    browser.waitForAngularEnabled(false);
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
    dashboardPage.fillPage3Form('Test', 'User', 'test@vmware.com', 'Test Org', 'systems_admin');
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
