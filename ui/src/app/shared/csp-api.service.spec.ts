/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { CspApiService } from './csp-api.service';
import { CSP_API_PREFIX } from './shared.config';

describe('CspApiService', () => {
  beforeEach(() => TestBed.configureTestingModule({
    imports: [HttpClientTestingModule],
    providers: [{
      provide: CSP_API_PREFIX, useValue: 'https://console.cloud.vmware.com/csp/gateway/am/api/auth/api-tokens/authorize'}, CspApiService]
}));

  it('should be created', inject([CspApiService], (service: CspApiService) => {
    expect(service).toBeTruthy();
  }));
});
