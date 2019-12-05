/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { TestBed } from '@angular/core/testing';

import { ContextualHelpService } from './contextual-help.service';

describe('ContextualHelpService', () => {
  beforeEach(() => TestBed.configureTestingModule({}));

  it('should be created', () => {
    const service: ContextualHelpService = TestBed.get(ContextualHelpService);
    expect(service).toBeTruthy();
  });
});
