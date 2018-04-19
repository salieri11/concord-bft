/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { AthenaApiService } from './athena-api.service';

describe('AthenaApiService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [AthenaApiService]
    });
  });

  it('should be created', inject([AthenaApiService], (service: AthenaApiService) => {
    expect(service).toBeTruthy();
  }));
});
