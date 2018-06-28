/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { OrgService } from './org.service';

describe('OrgService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [OrgService]
    });
  });

  it('should be created', inject([OrgService], (service: OrgService) => {
    expect(service).toBeTruthy();
  }));
});
