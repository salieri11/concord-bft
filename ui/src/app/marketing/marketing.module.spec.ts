/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { MarketingModule } from './marketing.module';

describe('MarketingModule', () => {
  let marketingModule: MarketingModule;

  beforeEach(() => {
    marketingModule = new MarketingModule();
  });

  it('should create an instance', () => {
    expect(marketingModule).toBeTruthy();
  });
});
