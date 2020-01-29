/*
 * Copyright 2019-2020 VMware, all rights reserved.
 */
import { TestBed } from '@angular/core/testing';

import { EnviornmentService } from './enviornment.service';

describe('EnviornmentService', () => {
  beforeEach(() => TestBed.configureTestingModule({}));

  it('should be created', () => {
    const service: EnviornmentService = TestBed.get(EnviornmentService);
    expect(service).toBeTruthy();
  });
});
