/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';

export class AppPage {
  navigateTo() {
    return browser.get('/');
  }

  getPageTitle() {
    return element(by.css('.branding .title')).getText();
  }
}
