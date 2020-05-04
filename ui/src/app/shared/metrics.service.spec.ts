/*
 * Copyright 2018-2020 VMware, all rights reserved.
 */
/* tslint:disable:no-unused-variable */

import { TestBed, inject, async } from '@angular/core/testing';
import { MetricsService } from './metrics.service';
import { getSpecTestingModule } from './shared-testing.module';

describe('Service: Metrics', () => {
  beforeEach(async( () => {
    const tester = getSpecTestingModule();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();
  }));

  it('should ...', inject([MetricsService], (service: MetricsService) => {
    expect(service).toBeTruthy();
  }));
});
