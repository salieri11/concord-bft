/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';
import { waitFor, waitInteractableFor, waitForURLContains } from '../helpers/utils';

// Safely injected credentials fron Jenkins (See BC-2712 for more information)
const CSP_LOGIN_EMAIL = browser.params.credentials.login.username;
const CSP_PASSWORD = browser.params.credentials.login.password;

export class CSPLogin {

  public static alreadyLoggedIn = false;

  navigateTo() {
    return browser.get('/');
  }

  // When on this page
  // https://console-stg.cloud.vmware.com/csp/gateway/discovery
  // login with email address
  fillInEmail() {
    const el = '#discovery_username';
    // Wait for email input to be interactable;
    // CSP decided to include input element in HTML while loading gif is going on.
    // So we have to actually wait for the login element to become interactable
    waitInteractableFor(el);
    browser.sleep(300);
    element(by.css(el)).click();
    browser.sleep(300);
    element(by.css(el)).sendKeys(CSP_LOGIN_EMAIL);
    browser.sleep(100);
    element(by.css('#next-btn-text')).click();
  }

  // Password page
  // OLD: https://csp-local.vidmpreview.com/SAAS/auth/login
  // CHANGED to https://csp-local.vidmpreview.com/authcontrol/auth/request (See BC-2697)
  fillInPassword() {
    const el = '#password';
    waitInteractableFor(el);
    browser.sleep(300);
    element(by.css(el)).click();
    browser.sleep(300);
    element(by.css(el)).sendKeys(CSP_PASSWORD);
    browser.sleep(100);
  }

  clickToAuthenticate() {
    // #signIn id in button is gone (See BC-2697)
    // element(by.css('#signIn')).click();
    browser.sleep(100);
    element(by.css('[type="submit"]')).click();
  }

  loginIfNotAlready() {
    if (CSPLogin.alreadyLoggedIn) {
      console.log('Already logged in, no need to login');
      return;
    }
    this.navigateTo();
    waitForURLContains('console-stg.cloud.vmware.com/csp/gateway/discovery');
    browser.sleep(500);
    this.fillInEmail();
    // URL Changed 5/13/2020 (See BC-2697)
    // waitForURLContains('csp-local.vidmpreview.com/SAAS/auth/login');
    waitForURLContains('csp-local.vidmpreview.com/authcontrol/auth/request');
    browser.sleep(1000);
    this.fillInPassword();
    this.clickToAuthenticate();
    CSPLogin.alreadyLoggedIn = true;
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
