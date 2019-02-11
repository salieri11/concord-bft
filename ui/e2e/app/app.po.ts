/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';

export class AppPage {
  logOut() {
    browser.waitForAngularEnabled(false);
    element(by.css('#profileMenu')).click();
    browser.waitForAngularEnabled(true);
    browser.waitForAngularEnabled(false);
    element(by.css('#logOutButton')).click();
    browser.waitForAngularEnabled(true);
  }

  getTourTitle() {
    return element(by.css('.ngxp-title'));
  }

  getTourNextButton() {
    return element(by.css('.ngxp-btn.btn-next'));
  }

  clickTourEndButton() {
    element(by.css('.ngxp-btn.btn-end')).click();
  }
}
