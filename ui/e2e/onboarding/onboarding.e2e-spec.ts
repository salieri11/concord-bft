/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, element, by } from 'protractor';

import { AuthHelper } from '../helpers/auth';
import { AppPage } from '../app/app.po';
import { LoginPage, CSPLogin } from '../login/login.po';
import { DashboardPage } from '../dashboard/dashboard.po';
import { OnboardingPage } from '../onboarding/onboarding.po';
import { DeployWizard } from '../deploy/deploy.po';
import { waitFor, waitForText, waitForURLContains } from '../helpers/utils';

describe('concord-ui Onboarding Flow', () => {
  let authHelper: AuthHelper;
  let appPage: AppPage;
  let deployWiz: DeployWizard;
  let dashboardPage: DashboardPage;
  let onboardingPage: OnboardingPage;
  let loginPage: CSPLogin;

  afterEach(() => {
    authHelper = new AuthHelper();
    authHelper.deleteSessionData();
  });

  beforeEach(() => {
    appPage = new AppPage();
    dashboardPage = new DashboardPage();
    onboardingPage = new OnboardingPage();
    deployWiz = new DeployWizard();
    browser.waitForAngularEnabled(false);
  });

  it('should login', () => {
    loginPage = new CSPLogin();
    loginPage.navigateTo();
    waitForURLContains('console-stg.cloud.vmware.com/csp/gateway/discovery');
    browser.sleep(500);
    loginPage.fillInEmail();
    // URL Changed 5/13/2020 (See BC-2697)
    // waitForURLContains('csp-local.vidmpreview.com/SAAS/auth/login');
    waitForURLContains('csp-local.vidmpreview.com/authcontrol/auth/request');
    browser.sleep(500);
    loginPage.fillInPassword();
  });

  it('should accept terms of service', () => {
    browser.sleep(2000);
    onboardingPage.readAndClickAccept('Reinhard', 'von Lohengramm', 'Galactic Empire');
    browser.sleep(1500);
  });

  it('should do the Ethereum tour', () => {
    browser.sleep(1000);
    appPage.goToConsortium().click();
    browser.sleep(1000);
    expect(appPage.getTourTitle().getText()).toEqual('General Status');
    browser.sleep(300);
    appPage.getTourNextButton().click();
    browser.sleep(300);
    expect(appPage.getTourTitle().getText()).toEqual('Committer List');
    appPage.getTourNextButton().click();
    browser.sleep(300);
    expect(appPage.getTourTitle().getText()).toEqual('Organizations');
    appPage.getTourNextButton().click();
    browser.sleep(300);
    expect(appPage.getTourTitle().getText()).toEqual('Manage Smart Contracts');
    appPage.getTourNextButton().click();
    browser.sleep(300);
    expect(appPage.getTourTitle().getText()).toEqual('Deploy');
    appPage.clickTourEndButton();
    browser.sleep(300);
    expect(appPage.getTourTitle().isDisplayed()).toBe(false);
    browser.waitForAngularEnabled(true);
  });


});
