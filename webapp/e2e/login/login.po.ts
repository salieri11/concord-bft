/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';

export class LoginPage {
  navigateTo() {
    return browser.get('/auth/login');
  }

  fillLogInForm(email, password) {
    element(by.css('#loginUsername')).sendKeys(email);
    element(by.css('#loginPassword')).sendKeys(password);
    element(by.css('.login-group button[type="submit"]')).click();
  }
}
