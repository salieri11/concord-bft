/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';
import { waitFor, waitForURLContains } from '../helpers/utils';

// Safely injected credentials fron Jenkins (See BC-2712 for more information)
const CSP_LOGIN_EMAIL = browser.params.credentials.login.username;
const CSP_PASSWORD = browser.params.credentials.login.password;

export class CSPLogin {
  navigateTo() {
    return browser.get('/');
  }

  // When on this page
  // https://console-stg.cloud.vmware.com/csp/gateway/discovery
  // login with email address
  fillInEmail() {
    const el = '#discovery_username';
    waitFor(el);
    element(by.css(el)).sendKeys(CSP_LOGIN_EMAIL);
    element(by.css('#next-btn-text')).click();
  }

  // Password page
  // OLD: https://csp-local.vidmpreview.com/SAAS/auth/login
  // CHANGED to https://csp-local.vidmpreview.com/authcontrol/auth/request (See BC-2697)
  fillInPassword() {
    const el = '#password';
    waitFor(el);
    browser.sleep(1000);
    element(by.css(el)).click();
    browser.sleep(300);
    element(by.css(el)).sendKeys(CSP_PASSWORD);
    browser.sleep(1000);
    // #signIn id in button is gone (See BC-2697)
    // element(by.css('#signIn')).click();
    element(by.css('[type="submit"]')).click();
  }


}


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
