/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';
import { HttpClient, HttpHandler } from '@angular/common/http';

import { NodesService } from './nodes.service';
import { ATHENA_API_PREFIX } from '../../shared/shared.config';

describe('NodesService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        NodesService,
        {provide: ATHENA_API_PREFIX, useValue: '/api/athena'},
        HttpClient,
        HttpHandler
      ]
    });
  });

  it('should be created', inject([NodesService], (service: NodesService) => {
    expect(service).toBeTruthy();
  }));
});
