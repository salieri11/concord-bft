/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser } from 'protractor';

import { SwaggerPage } from './swagger.po';
import { waitFor } from '../helpers/utils';

describe('concord-ui Swagger Docs', () => {
  let swaggerPage: SwaggerPage;

  beforeAll(() => {
    browser.waitForAngularEnabled(false);
  });

  beforeEach(() => {
    swaggerPage = new SwaggerPage();
    browser.waitForAngularEnabled(false);
    waitFor('#sidenav-apis');
    swaggerPage.navigateTo();
  });

  it('should navigate to the documentation page', () => {
    browser.sleep(2000);
    expect(swaggerPage.getPageTitle()).toContain('VMware Blockchain');
  });

  it('should expand a section of the documentation on click', () => {
    const consortiumListGetId = '#operations-profiles-getConsortiums';

    expect(swaggerPage.getOpBlockBodyForId(consortiumListGetId).isPresent()).toBe(false);
    swaggerPage.clickSwaggerHeaderForId(consortiumListGetId);
    expect(swaggerPage.getOpBlockBodyForId(consortiumListGetId).isPresent()).toBe(true);
  });

  it('should allow calling API endpoints from the UI', () => {
    const consortiumListGetId = '#operations-profiles-getConsortiums';
    const orgListGetId = '#operations-profiles-getOrganizations';

    // Test get nodes
    waitFor(`${consortiumListGetId} .btn.try-out__btn`);
    swaggerPage.clickTryItOutButtonForId(consortiumListGetId);
    expect(swaggerPage.getOpBlockExecuteButtonForId(consortiumListGetId).isPresent()).toBe(true);
    waitFor('.btn.execute.opblock-control__btn');
    swaggerPage.clickOpBlockExecuteButtonForId(consortiumListGetId);
    expect(swaggerPage.getResponseStatusForId(consortiumListGetId)).toEqual('200');
    // Clear responses
    waitFor('.btn.try-out__btn.cancel');
    swaggerPage.clickCancelTryItOutButtonForId(consortiumListGetId);
    expect(swaggerPage.getOpBlockExecuteButtonForId(consortiumListGetId).isPresent()).toBe(false);
    waitFor('.opblock-summary');
    swaggerPage.clickSwaggerHeaderForId(consortiumListGetId);
    // Test get blocks with a parameter
    swaggerPage.clickSwaggerHeaderForId(orgListGetId);
    waitFor(`${orgListGetId} .btn.try-out__btn`);
    swaggerPage.clickTryItOutButtonForId(orgListGetId);
    expect(swaggerPage.getOpBlockExecuteButtonForId(orgListGetId).isPresent()).toBe(true);
    waitFor(`${orgListGetId} .btn.execute.opblock-control__btn`);
    swaggerPage.clickOpBlockExecuteButtonForId(orgListGetId);
    waitFor(`${orgListGetId} .col.response-col_status`);
    expect(swaggerPage.getResponseStatusForId(orgListGetId)).toEqual('200');
    waitFor('.request-url pre');
    expect(swaggerPage.getRequestUrlFromResponse()).toContain('/api/organizations');
  });
});
