/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { TestBed, inject, async } from '@angular/core/testing';

import { NodesService } from './nodes.service';
import { getSpecTestingModule } from '../../shared/shared.module';
import { CONCORD_API_PREFIX } from '../../shared/shared.config';

describe('NodesService', () => {

  beforeEach(async( async () => {
    const tester = await getSpecTestingModule();
    tester.importLanguagePack();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [{provide: CONCORD_API_PREFIX, useValue: 'api/concord'}], declarations: []
    })).compileComponents();
  }));

  it('should be created', inject([NodesService], (service: NodesService) => {
    expect(service).toBeTruthy();
  }));
});
