/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { OrgService } from './org.service';
import { ANDES_API_PREFIX } from '../../shared/shared.config';

describe('OrgService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [
        OrgService,
        {provide: ANDES_API_PREFIX, useValue: 'api'},
      ]
    });
  });

  it('should be created', inject([OrgService], (service: OrgService) => {
    expect(service).toBeTruthy();
  }));
});
