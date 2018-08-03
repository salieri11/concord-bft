/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';

export class DashboardPage {
  navigateTo() {
    return browser.get('/dashboard');
  }

  getPageTitle() {
    return element(by.css('.branding .nav-link .title')).getText();
  }

  getBlockchainWizardForm() {
    return element(by.css('.blockchain-wizard .modal'));
  }

  getHeaderWithText(text) {
    return element(by.cssContainingText('.modal-header', text));
  }

  clickWizardNextButton() {
    element(by.cssContainingText('button.clr-wizard-btn--primary', 'Next')).click();
  }

  clickWizardFinishButton() {
    element(by.cssContainingText('button.clr-wizard-btn--primary.btn-success', 'Finish')).click();
  }

  clickAdvancedAccordionHeader() {
    element(by.css('.accordion-header .title-container')).click();
  }

  getOrgsTableCellWithText(text) {
    return element(by.cssContainingText('.orgs-table td', text));
  }

  getUsersTableCellWithText(text) {
    return element(by.cssContainingText('.users-table td', text));
  }

  getReviewRowWithText(text) {
    return element(by.cssContainingText('small.text-muted', text));
  }

  getAdvancedNetworkNameValue() {
    return element(by.css('#network-name')).getAttribute('value');
  }

  getAdvancedNumberOfNodesValue() {
    return element(by.css('#number-of-nodes')).getAttribute('value');
  }

  getAdvancedPublicNodesRegionsValueLength() {
    return element(by.css('#public-nodes-regions input')).getAttribute('value').then((value) => {
      return value.split(',').length;
    });
  }

  getAdvancedPrivateNodeValue() {
    return element(by.css('#private-node input')).getAttribute('value');
  }

  getTasks() {
    return element.all(by.css('athena-task'));
  }

  fillPage1Form(type, faultTolerance, consortiumName) {
    element(by.css('#blockchain-type')).sendKeys(type);
    element(by.css('#fault-tolerance')).sendKeys(faultTolerance);
    element(by.css('#consortium-name')).sendKeys(consortiumName);
  }

  fillPage2Form(orgName, orgLocation) {
    element(by.css('#org-name')).sendKeys(orgName);
    element(by.css('#org-location')).sendKeys(orgLocation);
    element(by.css('#add-org-button')).click();
  }

  fillPage3Form(firstName, lastName, email, org, role) {
    element(by.css('#user-first-name')).sendKeys(firstName);
    element(by.css('#user-last-name')).sendKeys(lastName);
    element(by.css('#user-email')).sendKeys(email);
    element(by.css('#user-org')).sendKeys(org);
    element(by.css('#user-role')).sendKeys(role);
    element(by.css('#add-user-button')).click();
  }
}
