/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';
import { HttpClient, HttpHandler } from '@angular/common/http';

import { SmartContractsService } from './smart-contracts.service';
import { ATHENA_API_PREFIX } from '../../shared/shared.config';

describe('SmartContractsService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        SmartContractsService,
        {provide: ATHENA_API_PREFIX, useValue: '/api/athena'},
        HttpClient,
        HttpHandler
      ]
    });
  });

  it('should be created', inject([SmartContractsService], (service: SmartContractsService) => {
    expect(service).toBeTruthy();
  }));
});
