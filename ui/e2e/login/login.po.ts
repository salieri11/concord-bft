/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';
import { waitFor, waitForURLContains } from '../helpers/utils';


const CSP_LOGIN_EMAIL = 'admin-blockchain-dev@csp.local';
const CSP_PASSWORD = 'Admin!23';

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
  // https://csp-local.vidmpreview.com/SAAS/auth/login
  fillInPassword() {
    const el = '#password';
    waitFor(el);
    element(by.css(el)).sendKeys(CSP_PASSWORD);
    element(by.css('#signIn')).click();
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
