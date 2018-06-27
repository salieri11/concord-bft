/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';

import { SmartContractsService } from './smart-contracts.service';

describe('SmartContractsService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [SmartContractsService]
    });
  });

  it('should be created', inject([SmartContractsService], (service: SmartContractsService) => {
    expect(service).toBeTruthy();
  }));
});
