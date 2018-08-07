/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { BlockGraphModule } from './block-graph.module';

describe('BlockGraphModule', () => {
  let blockGraphModule: BlockGraphModule;

  beforeEach(() => {
    blockGraphModule = new BlockGraphModule();
  });

  it('should create an instance', () => {
    expect(blockGraphModule).toBeTruthy();
  });
});
