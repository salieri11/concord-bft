/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';

export class OnboardingPage {
  navigateTo() {
    return browser.get('/auth/onboarding');
  }

  readAndClickAccept(firstName, lastName, company) {
    element(by.css('#firstName')).sendKeys(firstName);
    element(by.css('#lastName')).sendKeys(lastName);
    element(by.css('#company')).sendKeys(company);

    browser.executeScript('document.getElementById("agreementEl").scrollTo(0, 30000);').then(function () {
      element(by.id('accept')).click();
    });
  }

  expectationsAndNext() {
    element(by.id('next')).click();
  }

}
