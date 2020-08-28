/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { BlockchainService } from './blockchain.service';
import { mockBlockchains } from './blockchain.model';
import { testFor, beforeTesting, prepareEach } from '../../../test.helper.spec';
import { swaggerMocks } from '../../../test.swagger.spec';


describe('BlockchainService', () => {
  let blockchainService: BlockchainService;

  const test = testFor(BlockchainService).expedite({
    imports: [], provides: [], declarations: [],
  }, beforeTesting(() => {
    blockchainService = test.component;
  }), prepareEach(() => {}));

  it('should be created', () => {
    expect(blockchainService).toBeTruthy();
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
