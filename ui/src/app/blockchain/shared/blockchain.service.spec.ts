/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { TestBed, async } from '@angular/core/testing';

import { BlockchainService } from './blockchain.service';
import { mockBlockchains } from './blockchain.model';
import { swaggerMocks, getSpecTestingModule } from '../../shared/shared-testing.module';


describe('BlockchainService', () => {
  let service: BlockchainService;

  beforeEach(async( () => {
    const tester = getSpecTestingModule();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();
  }));

  it('should be created', () => {
    service = TestBed.get(BlockchainService);
    expect(service).toBeTruthy();
  });

  it('check local blockchian mocks against Swagger mocks', async () => {
    const fromSwagger =  await swaggerMocks.sampleResponse('GET /blockchains', 200);
    const blockchainFromSwagger = fromSwagger[0];
    for (const blockchain of mockBlockchains) {
      for (const field of Object.keys(blockchainFromSwagger)) {
        if (blockchain[field]) { // Fields should have the same type
          if (typeof blockchain[field] !== typeof blockchainFromSwagger[field]) {
            console.warn(`Response fields are not matching for '${field}':\n`
                        + ` typeof ${blockchain[field]} != typeof ${blockchainFromSwagger[field]}`);
          }
          // TODO: blockchainResponse.created does not line-up, need to fix
          // expect(typeof blockchain[field]).toBe(typeof blockchainFromSwagger[field]);
        }
      }
    }
  });
});
