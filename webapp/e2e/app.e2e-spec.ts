/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { AppPage } from './app.po';

describe('athena-ui App', () => {
  let page: AppPage;

  beforeEach(() => {
    page = new AppPage();
  });

  it('should display nav title', () => {
    page.navigateTo();
    expect(page.getParagraphText()).toEqual('Athena UI');
  });
});
