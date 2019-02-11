/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { ConsortiumService } from './consortium.service';
import { ANDES_API_PREFIX } from '../../shared/shared.config';

describe('ConsortiumService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [
        ConsortiumService,
        {provide: ANDES_API_PREFIX, useValue: 'api'},
      ]
    });
  });

  it('should be created', inject([ConsortiumService], (service: ConsortiumService) => {
    expect(service).toBeTruthy();
  }));
});
