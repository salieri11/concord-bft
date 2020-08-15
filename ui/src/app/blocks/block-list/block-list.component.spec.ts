/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { BlockListComponent } from './block-list.component';
import { testFor, prepareEach, beforeTesting } from '../../../test.helper.spec';
import { ClarityModule } from '@clr/angular';

describe('BlockListComponent', () => {
  const test = testFor(BlockListComponent).expedite({
    imports: [ClarityModule], provides: [], declarations: [BlockListComponent],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
