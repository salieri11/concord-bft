/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { EthApiService } from './eth-api.service';
import { ETHEREUM_API_PREFIX } from './shared.config';

describe('EthApiService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [
        {provide: ETHEREUM_API_PREFIX, useValue: 'api/athena/eth'},
        EthApiService
      ]
    });
  });

  it('should be created', inject([EthApiService], (service: EthApiService) => {
    expect(service).toBeTruthy();
  }));
});
