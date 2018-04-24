/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';

import { EthApiService } from './eth-api.service';

describe('EthApiService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [EthApiService]
    });
  });

  it('should be created', inject([EthApiService], (service: EthApiService) => {
    expect(service).toBeTruthy();
  }));
});
