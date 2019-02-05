/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';
import { HttpClient, HttpHandler } from '@angular/common/http';

import { NodesService } from './nodes.service';
import { CONCORD_API_PREFIX } from '../../shared/shared.config';

describe('NodesService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        NodesService,
        {provide: CONCORD_API_PREFIX, useValue: 'api/concord'},
        HttpClient,
        HttpHandler
      ]
    });
  });

  it('should be created', inject([NodesService], (service: NodesService) => {
    expect(service).toBeTruthy();
  }));
});
