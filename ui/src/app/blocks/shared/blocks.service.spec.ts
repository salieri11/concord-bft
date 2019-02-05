/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';
import { HttpClient, HttpHandler } from '@angular/common/http';

import { BlocksService } from './blocks.service';
import { CONCORD_API_PREFIX } from '../../shared/shared.config';

describe('BlocksService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        BlocksService,
        {provide: CONCORD_API_PREFIX, useValue: 'api/concord'},
        HttpClient,
        HttpHandler
      ]
    });
  });

  it('should be created', inject([BlocksService], (service: BlocksService) => {
    expect(service).toBeTruthy();
  }));
});
