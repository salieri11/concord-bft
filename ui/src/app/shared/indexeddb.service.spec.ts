/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { TestBed } from '@angular/core/testing';

import { IndexedDBService } from './indexeddb.service';

describe('IndexedDBService', () => {
  beforeEach(() => TestBed.configureTestingModule({}));

  it('should be created', () => {
    const service: IndexedDBService = TestBed.get(IndexedDBService);
    expect(service).toBeTruthy();
  });
});
