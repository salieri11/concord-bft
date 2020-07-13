/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { MockSharedModule } from './../../shared/shared.module';

import { LogApiService } from './log-api.service';
import { LOG_API_PREFIX } from '../../shared/shared.config';

describe('LogApiService', () => {
  beforeEach(() => TestBed.configureTestingModule({
    imports: [HttpClientTestingModule, MockSharedModule],
    providers: [
      {
        provide: LOG_API_PREFIX,
        useValue: 'https://api.mgmt.cloud.vmware.com',
      },
      LogApiService
    ]
  }));

  it('should be created', () => {
    const service: LogApiService = TestBed.get(LogApiService);
    expect(service).toBeTruthy();
  });
});
