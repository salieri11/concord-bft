/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';

import { HttpClient, HttpHandler } from '@angular/common/http';

import { BlockchainsService } from './blockchains.service';
import { ANDES_API_PREFIX } from '../../shared/shared.config';

describe('BlockchainsService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        BlockchainsService,
        {provide: ANDES_API_PREFIX, useValue: '/api'},
        HttpClient,
        HttpHandler
      ]
    });
  });

  it('should be created', inject([BlockchainsService], (service: BlockchainsService) => {
    expect(service).toBeTruthy();
  }));
});
