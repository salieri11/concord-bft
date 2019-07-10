/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, element } from 'protractor';

import { AuthHelper } from '../helpers/auth';
import { AppPage } from '../app/app.po';
import { LoginPage, CSPLogin } from '../login/login.po';
import { DashboardPage } from '../dashboard/dashboard.po';
import { waitFor, waitForText, waitForURLContains } from '../helpers/utils';

describe('concord-ui Onboarding Flow', () => {
  let authHelper: AuthHelper;
  let appPage: AppPage;
  let dashboardPage: DashboardPage;
  let loginPage: CSPLogin;

  afterEach(() => {
    authHelper = new AuthHelper();
    authHelper.deleteSessionData();
  });

  beforeEach(() => {
    appPage = new AppPage();
    dashboardPage = new DashboardPage();
    browser.waitForAngularEnabled(false);
    loginPage = new CSPLogin();
    loginPage.navigateTo();
    waitForURLContains('console-stg.cloud.vmware.com/csp/gateway/discovery');
    browser.sleep(500);
    loginPage.fillInEmail();
    waitForURLContains('csp-local.vidmpreview.com/SAAS/auth/login');
    browser.sleep(500);
    loginPage.fillInPassword();
  });

  it('should onboard to the org tour', () => {
    browser.sleep(3000);
    appPage.goToConsortium().click();
    browser.sleep(500);
    expect(appPage.getTourTitle().getText()).toEqual('General Status');
    browser.sleep(300);
    appPage.getTourNextButton().click();
    browser.sleep(300);
    expect(appPage.getTourTitle().getText()).toEqual('Contract List');
    appPage.getTourNextButton().click();
    browser.sleep(300);
    expect(appPage.getTourTitle().getText()).toEqual('Manage Smart contracts');
    appPage.clickTourEndButton();
    browser.sleep(300);
    expect(appPage.getTourTitle().isDisplayed()).toBe(false);
    browser.waitForAngularEnabled(true);
  });
});
