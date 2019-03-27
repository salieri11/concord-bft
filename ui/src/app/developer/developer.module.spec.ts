/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { DeveloperModule } from './developer.module';

describe('DeveloperModule', () => {
  let developerModule: DeveloperModule;

  beforeEach(() => {
    developerModule = new DeveloperModule();
  });

  it('should create an instance', () => {
    expect(developerModule).toBeTruthy();
  });
});
