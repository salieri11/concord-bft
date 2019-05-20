/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser } from 'protractor';

import { LoginPage } from '../login/login.po';
import { AppPage } from '../app/app.po';

export class AuthHelper {
  logIn(email, password) {
    const loginPage = new LoginPage();

    loginPage.navigateTo();
    loginPage.fillLogInForm(email, password);
  }

  deleteSessionData() {
    browser.executeScript('window.localStorage.clear();');
    browser.executeScript('window.sessionStorage.clear();');
  }
}
