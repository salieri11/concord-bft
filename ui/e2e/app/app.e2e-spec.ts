/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { AuthHelper } from '../helpers/auth';
import { LoginPage } from '../login/login.po';
import { MarketingPage } from '../marketing/marketing.po';
import { DashboardPage } from '../dashboard/dashboard.po';

describe('concord-ui App', () => {
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
  });

});
