/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { MockSharedModule } from './../../shared/shared.module';

import { BlockchainService } from './blockchain.service';
import { ConsortiumService } from './../../consortium/shared/consortium.service';

describe('BlockchainService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule, MockSharedModule],
      providers: [ConsortiumService]
    });
  });

  it('should be created', () => {
    const service: BlockchainService = TestBed.get(BlockchainService);
    expect(service).toBeTruthy();
  });
});
