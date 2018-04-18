/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { AppPage } from './app.po';

describe('athena-ui App', () => {
  let page: AppPage;

  beforeEach(() => {
    page = new AppPage();
  });

  it('should display the page title', () => {
    page.navigateTo();
    expect(page.getPageTitle()).toEqual('VMware Athena');
  });
});
