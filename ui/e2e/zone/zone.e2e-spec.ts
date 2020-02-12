/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, element, by } from 'protractor';

import { AuthHelper } from '../helpers/auth';
import { AppPage } from '../app/app.po';
import { LoginPage, CSPLogin } from '../login/login.po';
import { DashboardPage } from '../dashboard/dashboard.po';
import { OnboardingPage } from '../onboarding/onboarding.po';
import { Zone } from './zone.po';
import { waitFor, waitForText, waitForURLContains } from '../helpers/utils';

describe('concord-ui Add Zone', () => {
  let authHelper: AuthHelper;
  let appPage: AppPage;
  let zone: Zone;
  let dashboardPage: DashboardPage;
  let onboardingPage: OnboardingPage;
  let loginPage: CSPLogin;

  afterEach(() => {
    authHelper = new AuthHelper();
  });

  beforeEach(() => {
    appPage = new AppPage();
    dashboardPage = new DashboardPage();
    onboardingPage = new OnboardingPage();
    zone = new Zone();
    browser.waitForAngularEnabled(false);
  });

  it('should login', () => {
    loginPage = new CSPLogin();
    loginPage.navigateTo();
    waitForURLContains('console-stg.cloud.vmware.com/csp/gateway/discovery');
    browser.sleep(500);
    loginPage.fillInEmail();
    waitForURLContains('csp-local.vidmpreview.com/SAAS/auth/login');
    browser.sleep(1000);
    loginPage.fillInPassword();
  });

  it('should accept terms of service', () => {
    browser.sleep(2000);
    onboardingPage.readAndClickAccept('Reinhard', 'von Lohengramm', 'Galactic Empire');
    browser.sleep(1500);
  });

  it('should add a vcenter form', () => {
    const title = 'DAML e2e';

    browser.sleep(2000);
    appPage.goToConsortium().click();
    browser.sleep(2000);
    appPage.goToZoneList();
    browser.sleep(800);
    zone.openNewZone();
    browser.sleep(1000);
    const message = zone.fillOutVcenter();
    waitFor('#onPremSuccess');
    expect(zone.getSuccessMessage()).toBe('Connection successful to your on premises vCenter.');

  });

  it('should fill in the rest of the form add and update', () => {
    browser.sleep(100);
    zone.fillOutRest();
    zone.add();
    waitFor('#updateZone');
    zone.update();
    waitFor('.test-description');
    browser.sleep(100);
    expect(zone.toastDescription()).toBe('Zone: Test - A');
  });


});
