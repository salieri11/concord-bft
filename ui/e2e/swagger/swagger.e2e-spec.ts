/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser } from 'protractor';

import { SwaggerPage } from './swagger.po';
import { AuthHelper } from '../helpers/auth';
import { LoginPage } from '../login/login.po';

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
    swaggerPage.clickSwaggerHeaderForId(memberListGetId);
    browser.sleep(500);
    swaggerPage.clickTryItOutButtonForId(memberListGetId);
    expect(swaggerPage.getOpBlockExecuteButtonForId(memberListGetId).isPresent()).toBe(true);
    swaggerPage.clickOpBlockExecuteButtonForId(memberListGetId);
    expect(swaggerPage.getResponseStatusForId(memberListGetId)).toEqual('200');
    expect(swaggerPage.getResponseTextForId(memberListGetId)).toContain('Response headers');
    // Clear responses
    swaggerPage.clickCancelTryItOutButtonForId(memberListGetId);
    expect(swaggerPage.getOpBlockExecuteButtonForId(memberListGetId).isPresent()).toBe(false);
    swaggerPage.clickSwaggerHeaderForId(memberListGetId);
    // Test get blocks with a parameter
    swaggerPage.clickSwaggerHeaderForId(blocksListGetId);
    browser.sleep(500);
    swaggerPage.clickTryItOutButtonForId(blocksListGetId);
    expect(swaggerPage.getOpBlockExecuteButtonForId(blocksListGetId).isPresent()).toBe(true);
    swaggerPage.getInputForAttribute('count').sendKeys(1);
    swaggerPage.clickOpBlockExecuteButtonForId(blocksListGetId);
    browser.sleep(2000);
    expect(swaggerPage.getResponseStatusForId(blocksListGetId)).toEqual('200');
    expect(swaggerPage.getRequestUrlFromResponse()).toContain('/api/concord/blocks?count=1');
  });
});
