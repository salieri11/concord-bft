/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { AthenaApiService } from './athena-api.service';
import { ATHENA_API_PREFIX } from './shared.config';

describe('AthenaApiService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [
        {provide: ATHENA_API_PREFIX, useValue: '/api/athena'},
        AthenaApiService
      ]
    });
  });

  it('should be created', inject([AthenaApiService], (service: AthenaApiService) => {
    expect(service).toBeTruthy();
  }));
});
