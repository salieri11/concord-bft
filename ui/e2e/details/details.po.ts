/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';
import { waitFor, textContentOf } from '../helpers/utils';

export class DetailsPage {

  async getAboutPageInfoValues() {
    return {
      organizationId: await textContentOf('details-page-organization-id'),
      consortiumId: await textContentOf('details-page-consortium-id'),
      blockchainId: await textContentOf('details-page-blockchain-id'),
      productVersion: await textContentOf('details-page-product-version'),
      createdBy: await textContentOf('details-page-created-by'),
      createdDate: await textContentOf('details-page-created-date'),
    };
  }

  async getValuesOfCommitterIndex(index: number = 0) {
    return {
      id: await textContentOf(`details-page-committer-${index}-id`),
      publicIp: await textContentOf(`details-page-committer-${index}-public-ip`),
      privateIp: await textContentOf(`details-page-committer-${index}-private-ip`),
      rpcUrl: await textContentOf(`details-page-committer-${index}-rpc-url`),
      zoneId: await textContentOf(`details-page-committer-${index}-zone-id`),
      zoneHumanReadable: await textContentOf(`details-page-committer-${index}-zone-human-readable`),
    };
  }

  async getValuesOfClientIndex(index: number = 0) {
    return {
      id: await textContentOf(`details-page-client-${index}-id`),
      publicIp: await textContentOf(`details-page-client-${index}-public-ip`),
      privateIp: await textContentOf(`details-page-client-${index}-private-ip`),
      authUrl: await textContentOf(`details-page-client-${index}-auth-url`),
      url: await textContentOf(`details-page-client-${index}-url`),
      zoneId: await textContentOf(`details-page-client-${index}-zone-id`),
      zoneHumanReadable: await textContentOf(`details-page-client-${index}-zone-human-readable`),
    };
  }

  goToAboutTab() {
    element(by.css('#details-page-about-tab-button')).click();
  }

  goToCommittersTab() {
    element(by.css('#details-page-committers-tab-button')).click();
  }

  goToClientsTab() {
    element(by.css('#details-page-clients-tab-button')).click();
  }

}
