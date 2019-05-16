/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, element } from 'protractor';

import { AuthHelper } from '../helpers/auth';
import { AppPage } from '../app/app.po';
import { LoginPage } from '../login/login.po';
import { OnboardingPage } from './onboarding.po';
import { DashboardPage } from '../dashboard/dashboard.po';
import { waitFor, waitForText } from '../helpers/utils';

describe('concord-ui Onboarding Flow', () => {
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
    browser.sleep(200);
    onboardingPage.expectationsAndNext();
    loginPage.fillLogInForm('admin@blockchain.local', 'Admin!23');
    browser.sleep(500);
    waitForText(element(by.cssContainingText('.title h3', 'Change Password')));
    loginPage.changePassword('T3sting!', 'T3sting!!');
    expect(loginPage.getChangeSubmit().getAttribute('disabled')).toBe('true');
    loginPage.changePassword('T3sting!', 'T3sting!');
    browser.sleep(200);
    loginPage.changePasswordSubmit();
    browser.sleep(200);
    browser.waitForAngularEnabled(false);
    waitFor('#go');
    appPage.goToConsortium().click();
    browser.sleep(1000);
    expect(appPage.getTourTitle().getText()).toEqual('General Status');
    browser.sleep(200);
    appPage.getTourNextButton().click();
    browser.sleep(200);
    expect(appPage.getTourTitle().getText()).toEqual('Contract List');
    appPage.getTourNextButton().click();
    browser.sleep(200);
    expect(appPage.getTourTitle().getText()).toEqual('Manage Smart contracts');
    appPage.getTourNextButton().click();
    browser.sleep(200);
    expect(appPage.getTourTitle().getText()).toEqual('Deploy');
    appPage.getTourNextButton().click();
    browser.sleep(200);
    expect(appPage.getTourTitle().getText()).toEqual('Manage users');
    appPage.getTourNextButton().click();
    browser.sleep(200);
    expect(appPage.getTourNextButton().isDisplayed()).toBe(false);
    appPage.clickTourEndButton();
    browser.sleep(200);
    expect(appPage.getTourTitle().isDisplayed()).toBe(false);
    browser.waitForAngularEnabled(true);
  });
});
