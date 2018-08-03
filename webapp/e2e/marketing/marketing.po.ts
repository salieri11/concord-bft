/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';

export class MarketingPage {
  navigateTo() {
    return browser.get('/');
  }

  clickLoginButton() {
    element(by.css('#login-button')).click();
  }

  clickSignUpButton() {
    element(by.css('.btn.call-to-action')).click();
  }

  getSignUpForm() {
    return element(by.css('#signup-form'));
  }
}
