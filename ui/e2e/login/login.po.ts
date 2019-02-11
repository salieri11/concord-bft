/*
 * Copyright 2018-2019 VMware, all rights reserved.
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

  changePassword(newPassword, confirmPassword) {
    element(by.css('#newPassword')).clear();
    element(by.css('#newPassword')).sendKeys(newPassword);
    element(by.css('#confirmPassword')).clear();
    element(by.css('#confirmPassword')).sendKeys(confirmPassword);
  }

  getChangeSubmit() {
    return element(by.css('#changePassword'));
  }
  changePasswordSubmit() {
    element(by.css('#changePassword')).click();
  }

}
