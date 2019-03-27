/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { TestBed } from '@angular/core/testing';
import { HttpClient, HttpHandler } from '@angular/common/http';

import { DappService } from './dapp.service';

describe('DeployDappService', () => {
  beforeEach(() => TestBed.configureTestingModule({
    providers: [HttpClient, HttpHandler]
  }));

  it('should be created', () => {
    const service: DappService = TestBed.get(DappService);
    expect(service).toBeTruthy();
  });
});
