/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { MockSharedModule } from '../../shared/shared.module';

import { BlocksService } from './blocks.service';

describe('BlocksService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [ MockSharedModule ],
      providers: [
        BlocksService,
        HttpClient,
        HttpHandler,
      ]
    });
  });

  it('should be created', inject([BlocksService], (service: BlocksService) => {
    expect(service).toBeTruthy();
  }));
});
