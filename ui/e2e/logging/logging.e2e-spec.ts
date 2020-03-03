/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, protractor, ProtractorExpectedConditions } from 'protractor';

import { LoggingPage } from './logging.po';
import { AuthHelper } from '../helpers/auth';
import { LoginPage } from '../login/login.po';
import { BROWSER_WAIT_TIME } from '../helpers/constants';

describe('concord-ui Logging', () => {
  let authHelper: AuthHelper;
  let loginPage: LoginPage;
  let loggingPage: LoggingPage;
  let until: ProtractorExpectedConditions;

  beforeAll(() => {
    until = protractor.ExpectedConditions;
    loginPage = new LoginPage();
    authHelper = new AuthHelper();
    loginPage.navigateTo();
    loginPage.fillLogInForm('admin@blockchain.local', 'T3sting!');
    browser.sleep(1000);
  });

  beforeEach(() => {
    loggingPage = new LoggingPage();
    loggingPage.navigateTo();
  });

  it('should have a concord option and wait for logs', () => {
    const threeMinutes = 3 * 60 * 1000;
    expect(loggingPage.getConcordOption().getText()).toBe('Concord');
    browser.sleep(threeMinutes);
  });

  it('should select concord option', () => {
    loggingPage.getConcordOption().click();
  });

  it('should load more items when clicking load more', () => {
    expect(loggingPage.getLoadMoreButton().isPresent()).toBe(true);
    // expect the count of items to be 20
    expect(loggingPage.getLogItemRowCount()).toBe(100);
    // Lint pagination is broken
    // click the next page button
    expect(loggingPage.getLoadMoreButton().isEnabled()).toBe(true);
    loggingPage.getLoadMoreButton().click();
    // Once the button is enabled again, the count should be 40
    browser
      .wait(until.elementToBeClickable(loggingPage.getLoadMoreButton()), BROWSER_WAIT_TIME);
    expect(loggingPage.getLogItemRowCount()).toBe(200);
  });

  it('should load a different time series on selection', () => {
    // total count will be present after initial load
    browser.wait(until.presenceOf(loggingPage.getTotalCount()), BROWSER_WAIT_TIME);
    // expect x and y axis labels to be day and hour
    expect(loggingPage.getHeatMapXAxisLabelText()).toBe('Day');
    expect(loggingPage.getHeatMapYAxisLabelText()).toBe('Hour');
    loggingPage.getTimeSelectionButton().click();
    // click on 6 hours
    expect(loggingPage.getSixHourTimeButton().isPresent()).toBe(true);
    loggingPage.getSixHourTimeButton().click();
    // after load, expect x and y axis labels to be hour and minute
    browser.wait(until.presenceOf(loggingPage.getTotalCount()), BROWSER_WAIT_TIME);
    browser.sleep(5000);
    expect(loggingPage.getHeatMapXAxisLabelText()).toBe('Hour');
    expect(loggingPage.getHeatMapYAxisLabelText()).toBe('Minute');
  });
});
