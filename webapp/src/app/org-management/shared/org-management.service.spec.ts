/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { OrgManagementService } from './org-management.service';
import { ANDES_API_PREFIX } from '../../shared/shared.config';

describe('OrgManagementService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [
        OrgManagementService,
        {provide: ANDES_API_PREFIX, useValue: '/api'},
      ]
    });
  });

  it('should be created', inject([OrgManagementService], (service: OrgManagementService) => {
    expect(service).toBeTruthy();
  }));
});
