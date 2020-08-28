/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { testFor, beforeTesting, prepareEach } from '../../../test.helper.spec';
import { BlockComponent } from './block.component';

describe('BlockComponent', () => {

  const test = testFor(BlockComponent).expedite({
    imports: [], provides: [], declarations: [BlockComponent],
  }, beforeTesting(() => {}), prepareEach(() => { }));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });

});
