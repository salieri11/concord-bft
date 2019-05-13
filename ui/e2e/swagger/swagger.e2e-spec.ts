/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser } from 'protractor';

import { SwaggerPage } from './swagger.po';
import { AuthHelper } from '../helpers/auth';
import { LoginPage } from '../login/login.po';
import { waitFor } from '../helpers/utils';

describe('concord-ui Swagger Docs', () => {
  let authHelper: AuthHelper;
  let loginPage: LoginPage;
  let swaggerPage: SwaggerPage;

  beforeAll(() => {
    loginPage = new LoginPage();
    authHelper = new AuthHelper();
    loginPage.navigateTo();
    loginPage.fillLogInForm('admin@blockchain.local', 'T3sting!');
    browser.sleep(1000);
  });

  afterAll(() => {
    authHelper.logOut();
  });

  beforeEach(() => {
    swaggerPage = new SwaggerPage();
    browser.waitForAngularEnabled(false);
    waitFor('#apis');
    swaggerPage.navigateTo();
  });

  it('should navigate to the documentation page', () => {
    expect(swaggerPage.getPageTitle()).toContain('VMware Blockchain');
  });

  it('should expand a section of the documentation on click', () => {
    const memberListGetId = '#operations-default-memberListGet';
    expect(swaggerPage.getOpBlockBodyForId(memberListGetId).isPresent()).toBe(false);
    swaggerPage.clickSwaggerHeaderForId(memberListGetId);
    expect(swaggerPage.getOpBlockBodyForId(memberListGetId).isPresent()).toBe(true);
  });

  it('should allow calling API endpoints from the UI', () => {
    const memberListGetId = '#operations-default-memberListGet';
    const blocksListGetId = '#operations-default-blockListGet';

    // Test get nodes
    // waitFor(`${memberListGetId} .opblock-summary`);
    // swaggerPage.clickSwaggerHeaderForId(memberListGetId);
    waitFor(`${memberListGetId} .btn.try-out__btn`);
    swaggerPage.clickTryItOutButtonForId(memberListGetId);
    expect(swaggerPage.getOpBlockExecuteButtonForId(memberListGetId).isPresent()).toBe(true);
    waitFor('.btn.execute.opblock-control__btn');
    swaggerPage.clickOpBlockExecuteButtonForId(memberListGetId);
    expect(swaggerPage.getResponseStatusForId(memberListGetId)).toEqual('200');
    expect(swaggerPage.getResponseTextForId(memberListGetId)).toContain('Response headers');
    // Clear responses
    waitFor('.btn.try-out__btn.cancel');
    swaggerPage.clickCancelTryItOutButtonForId(memberListGetId);
    expect(swaggerPage.getOpBlockExecuteButtonForId(memberListGetId).isPresent()).toBe(false);
    waitFor('.opblock-summary');
    swaggerPage.clickSwaggerHeaderForId(memberListGetId);
    // Test get blocks with a parameter
    swaggerPage.clickSwaggerHeaderForId(blocksListGetId);
    waitFor(`${blocksListGetId} .btn.try-out__btn`);
    swaggerPage.clickTryItOutButtonForId(blocksListGetId);
    expect(swaggerPage.getOpBlockExecuteButtonForId(blocksListGetId).isPresent()).toBe(true);
    swaggerPage.getInputForAttribute('count').sendKeys(1);
    waitFor(`${blocksListGetId} .btn.execute.opblock-control__btn`);
    swaggerPage.clickOpBlockExecuteButtonForId(blocksListGetId);
    waitFor(`${blocksListGetId} .col.response-col_status`);
    expect(swaggerPage.getResponseStatusForId(blocksListGetId)).toEqual('200');
    waitFor('.request-url pre');
    expect(swaggerPage.getRequestUrlFromResponse()).toContain('/api/concord/blocks?count=1');
  });
});
