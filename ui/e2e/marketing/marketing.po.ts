/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';

export class MarketingPage {
  navigateTo() {
    return browser.get('/');
  }

  clickLoginButton() {
    element(by.css('#loginButton')).click();
  }

  clickSignUpButton() {
    element(by.css('.btn.call-to-action')).click();
  }

  getSignUpForm() {
    return element(by.css('#signupForm'));
  }
}
