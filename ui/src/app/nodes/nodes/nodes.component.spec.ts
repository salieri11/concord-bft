/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { NodesComponent } from './nodes.component';
import { testFor, prepareEach, beforeTesting } from '../../../test.helper.spec';

describe('NodesComponent', () => {
  const test = testFor(NodesComponent).expedite({
    imports: [], provides: [], declarations: [NodesComponent],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
