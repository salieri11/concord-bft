/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';

import { GlobalErrorHandlerService, ErrorAlertService } from './global-error-handler.service';

describe('GlobalErrorHandlerService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [GlobalErrorHandlerService, ErrorAlertService]
    });
  });

  it('should be created', inject([GlobalErrorHandlerService], (service: GlobalErrorHandlerService) => {
    expect(service).toBeTruthy();
  }));

  it('should be created', inject([ErrorAlertService], (service: ErrorAlertService) => {
    expect(service).toBeTruthy();
  }));

});

