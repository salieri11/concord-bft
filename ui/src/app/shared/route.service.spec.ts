/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, TestBed } from '@angular/core/testing';

import { RouteService } from './route.service';
import { getSpecTestingModule } from './shared.module';

describe('RouteService', () => {
  beforeEach(async( async () => {
    const tester = await getSpecTestingModule();
    tester.provideActivatedRoute();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();
  }));

  it('should be created', () => {
    const service: RouteService = TestBed.get(RouteService);
    expect(service).toBeTruthy();
  });
});
