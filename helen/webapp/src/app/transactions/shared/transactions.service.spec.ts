/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';
import { HttpClient, HttpHandler } from '@angular/common/http';

import { TransactionsService } from './transactions.service';
import { ATHENA_API_PREFIX } from '../../shared/shared.config';

describe('TransactionsService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        TransactionsService,
        {provide: ATHENA_API_PREFIX, useValue: 'api/athena'},
        HttpClient,
        HttpHandler
      ]
    });
  });

  it('should be created', inject([TransactionsService], (service: TransactionsService) => {
    expect(service).toBeTruthy();
  }));
});
