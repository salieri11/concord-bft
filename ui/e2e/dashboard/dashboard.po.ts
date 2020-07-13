/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';

export class DashboardPage {
  navigateTo() {
    return browser.get('/');
  }

  navigateToDeploy() { // This function is unreferenced anywhere
    element(by.id('menu')).click();
    browser.sleep(200);
    element(by.id('sidenav-deploy')).click();
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
    return element(by.css('#networkName')).getAttribute('value');
  }

  getAdvancedNumberOfNodesValue() {
    return element(by.css('#numberOfNodes')).getAttribute('value');
  }

  getAdvancedPublicNodesRegionsValueLength() {
    return element(by.css('#publicNodesRegions input')).getAttribute('value').then((value) => {
      return value.split(',').length;
    });
  }

  getAdvancedPrivateNodeValue() {
    return element(by.css('#privateNode input')).getAttribute('value');
  }

  getTasks() {
    return element.all(by.css('concord-task'));
  }

  fillPage1Form(type, faultTolerance, consortiumName) {
    element(by.css('#blockchainType')).sendKeys(type);
    element(by.css('#faultTolerance')).sendKeys(faultTolerance);
    element(by.css('#consortiumName')).sendKeys(consortiumName);
  }

  fillPage2Form(orgName, orgLocation) {
    element(by.css('#orgName')).sendKeys(orgName);
    element(by.css('#orgLocation')).sendKeys(orgLocation);
    element(by.css('#addOrgButton')).click();
  }

  fillPage3Form(firstName, lastName, email, org, role) {
    element(by.css('#userFirstName')).sendKeys(firstName);
    element(by.css('#userLastName')).sendKeys(lastName);
    element(by.css('#userEmail')).sendKeys(email);
    element(by.css('#userOrg')).sendKeys(org);
    element(by.css('#userRole')).sendKeys(role);
    element(by.css('#addUserButton')).click();
  }

  switchToDefault() {
    element(by.css('.consortium-select option:nth-child(1)')).click();
  }

  startTour() {
    element(by.css('.guides')).click();
    browser.sleep(300);
    element(by.css('.guides-tour')).click();
  }

}
