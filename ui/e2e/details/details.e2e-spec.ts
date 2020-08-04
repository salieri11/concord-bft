/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, element, by } from 'protractor';

import { AppPage } from '../app/app.po';
import { DetailsPage } from './details.po';
import { waitFor, waitForText, waitForURLContains, uuidLaxRegExp } from '../helpers/utils';

describe('concord-ui Details Page', () => {
  let appPage: AppPage;
  let detailsPage: DetailsPage;

  beforeEach(() => {
    appPage = new AppPage();
    detailsPage = new DetailsPage();
    browser.waitForAngularEnabled(false);
  });

  it('should be able to go to details page', async () => {
    browser.sleep(1000);
    appPage.goToDetailsPage();
    browser.sleep(800);
    waitFor('#details-page-ready'); // Page is set with all data
  });

  it('should display basic blockchain info', async () => {
    const values = await detailsPage.getAboutPageInfoValues();
    expect(values.organizationId).toMatch(uuidLaxRegExp); // must be UUID
    expect(values.consortiumId).toMatch(uuidLaxRegExp);
    expect(values.blockchainId).toMatch(uuidLaxRegExp);
    expect(values.productVersion.length).toBeGreaterThanOrEqual(2);
    expect(values.createdBy.length).toBeGreaterThanOrEqual(2);
    expect(values.createdDate.length).toBeGreaterThanOrEqual(10); // Some date; e.g. Jun 18, 2020, 9:08:03 PM
  });

  it('should display committer info', async () => {
    browser.sleep(500);
    detailsPage.goToCommittersTab();
    browser.sleep(1500);
    const values = await detailsPage.getValuesOfCommitterIndex(0); // First committer
    expect(values.id).toMatch(uuidLaxRegExp); // must be UUID
    expect(values.zoneId).toMatch(uuidLaxRegExp);
    const hasAtLeastOneIP = values.publicIp || values.privateIp;
    expect(hasAtLeastOneIP).toBeTruthy(); // must have at least one ip
  });

  // ! Skipped with xit; E2E doesn't have DAML fixture yet
  xit('should display clients info', async () => {
    browser.sleep(500);
    detailsPage.goToClientsTab();
    browser.sleep(1500);
    const values = await detailsPage.getValuesOfClientIndex(0); // First client
    expect(values.id).toMatch(uuidLaxRegExp); // must be UUID
    expect(values.groupId).toMatch(uuidLaxRegExp);
    expect(values.zoneId).toMatch(uuidLaxRegExp);
    const hasAtLeastOneIP = values.publicIp || values.privateIp;
    expect(hasAtLeastOneIP).toBeTruthy(); // must have at least one ip
    expect(values.url.length).toBeGreaterThan(1); // must have endpoint url (e.g. https://1.1.1.1:6865)
  });

});
