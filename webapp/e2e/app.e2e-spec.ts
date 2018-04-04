/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { AppPage } from './app.po';

describe('athena-ui App', () => {
  let page: AppPage;

  beforeEach(() => {
    page = new AppPage();
  });

  it('should display logo with alt', () => {
    page.navigateTo();
    expect(page.getLogo().getAttribute('alt')).toEqual('VMware');
  });
});
