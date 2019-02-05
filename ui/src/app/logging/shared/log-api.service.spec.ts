/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { LogApiService } from './log-api.service';
import { CspApiService } from '../../shared/csp-api.service';
import { LOG_API_PREFIX, CSP_API_PREFIX } from '../../shared/shared.config';

describe('LogApiService', () => {
  beforeEach(() => TestBed.configureTestingModule({
    imports: [HttpClientTestingModule],
    providers: [
      {
        provide: LOG_API_PREFIX,
        useValue: 'https://api.mgmt.cloud.vmware.com',
      },
      {
        provide: CSP_API_PREFIX,
        useValue: 'https://console.cloud.vmware.com/csp/gateway/am/api/auth/api-tokens/authorize'
      },
      LogApiService,
      CspApiService
    ]
  }));

  it('should be created', () => {
    const service: LogApiService = TestBed.get(LogApiService);
    expect(service).toBeTruthy();
  });
});
