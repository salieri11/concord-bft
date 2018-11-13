/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { SwaggerModule } from './swagger.module';

describe('SwaggerModule', () => {
  let swaggerModule: SwaggerModule;

  beforeEach(() => {
    swaggerModule = new SwaggerModule();
  });

  it('should create an instance', () => {
    expect(swaggerModule).toBeTruthy();
  });
});
