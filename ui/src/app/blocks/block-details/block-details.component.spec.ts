/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { BlockDetailsComponent } from './block-details.component';
import { testFor, prepareEach, beforeTesting } from '../../../test.helper.spec';

describe('BlockDetailsComponent', () => {
  const test = testFor(BlockDetailsComponent).expedite({
    imports: [], provides: [], declarations: [BlockDetailsComponent],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
