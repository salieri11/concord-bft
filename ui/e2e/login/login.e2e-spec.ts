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
    loginPage.loginIfNotAlready();
  });

});
