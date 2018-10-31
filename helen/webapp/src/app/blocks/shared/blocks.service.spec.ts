/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';
import { HttpClient, HttpHandler } from '@angular/common/http';

import { BlocksService } from './blocks.service';
import { ATHENA_API_PREFIX } from '../../shared/shared.config';

describe('BlocksService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        BlocksService,
        {provide: ATHENA_API_PREFIX, useValue: 'api/athena'},
        HttpClient,
        HttpHandler
      ]
    });
  });

  it('should be created', inject([BlocksService], (service: BlocksService) => {
    expect(service).toBeTruthy();
  }));
});
