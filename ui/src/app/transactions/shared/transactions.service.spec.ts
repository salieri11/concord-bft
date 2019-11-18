/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { MockSharedModule } from '../../shared/shared.module';

import { TransactionsService } from './transactions.service';

describe('TransactionsService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [ MockSharedModule ],
      providers: [
        TransactionsService,
        HttpClient,
        HttpHandler
      ]
    });
  });

  it('should be created', inject([TransactionsService], (service: TransactionsService) => {
    expect(service).toBeTruthy();
  }));
});
