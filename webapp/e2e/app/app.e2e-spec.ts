/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { browser } from 'protractor';

import { AuthHelper } from '../helpers/auth';
import { LoginPage } from '../login/login.po';
import { MarketingPage } from '../marketing/marketing.po';
import { DashboardPage } from '../dashboard/dashboard.po';

describe('athena-ui App', () => {
  let authHelper: AuthHelper;
  let loginPage: LoginPage;
  let dashboardPage: DashboardPage;
  let marketingPage: MarketingPage;

  beforeEach(() => {
    loginPage = new LoginPage();
    dashboardPage = new DashboardPage();
    marketingPage = new MarketingPage();
  });

  afterAll(() => {
    authHelper = new AuthHelper();
    authHelper.logOut();
  });

  it('should display the page title', () => {
    browser.waitForAngularEnabled(false);
    marketingPage.navigateTo();
    marketingPage.clickLoginButton();
    loginPage.fillLogInForm('test@vmware.com', 'password');
    expect(dashboardPage.getPageTitle()).toEqual('VMware Athena');
    browser.waitForAngularEnabled(true);
  });
});
