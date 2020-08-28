/*
 * Copyright 2018-2020 VMware, all rights reserved.
 */

import { testFor, beforeTesting, prepareEach } from '../../../test.helper.spec';

import { NodeSizingComponent } from './node-sizing.component';

describe('NodeSizingComponent', () => {

  const test = testFor(NodeSizingComponent).expedite({
    imports: [], provides: [], declarations: [NodeSizingComponent],
  }, beforeTesting(() => {}), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });

});
