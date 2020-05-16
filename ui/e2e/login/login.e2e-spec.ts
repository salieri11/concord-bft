/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, element, by } from 'protractor';

import { AppPage } from '../app/app.po';
import { LoginPage, CSPLogin } from './login.po';
import { OnboardingPage } from '../onboarding/onboarding.po';
import { waitFor, waitForText, waitForURLContains } from '../helpers/utils';

describe('concord-ui Login and Accept Terms', () => {
  let appPage: AppPage;
  let onboardingPage: OnboardingPage;
  let loginPage: CSPLogin;

  beforeEach(() => {
    appPage = new AppPage();
    onboardingPage = new OnboardingPage();
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
    browser.sleep(1000);
    loginPage.fillInPassword();
  });

  it('should accept terms of service', () => {
    browser.sleep(2000);
    onboardingPage.readAndClickAccept('Reinhard', 'von Lohengramm', 'Galactic Empire');
    browser.sleep(3500);
    appPage.goToConsortium().click();
  });

});
