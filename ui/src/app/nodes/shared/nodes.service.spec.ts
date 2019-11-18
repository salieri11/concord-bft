/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { TestBed, inject, async } from '@angular/core/testing';

import { NodesService } from './nodes.service';
import { getSpecTestingModule } from '../../shared/shared-testing.module';

describe('NodesService', () => {

  beforeEach(async( () => {
    const tester = getSpecTestingModule();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();
  }));

  it('should be created', inject([NodesService], (service: NodesService) => {
    expect(service).toBeTruthy();
  }));
});
