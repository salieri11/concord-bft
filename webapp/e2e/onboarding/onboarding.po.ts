/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';

export class OnboardingPage {
  navigateTo() {
    return browser.get('/auth/onboarding');
  }

  clickSetupOrg() {
    element(by.cssContainingText('.card-header', 'Setup Organization')).click();
  }

  clickDeployBlockchain() {
    element(by.cssContainingText('.card-header', 'Deploy Blockchain')).click();
  }
}
