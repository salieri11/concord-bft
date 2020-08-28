/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { NodesService } from './nodes.service';
import { testFor, beforeTesting, prepareEach } from '../../../test.helper.spec';

describe('NodesService', () => {
  const test = testFor(NodesService).expedite({
    imports: [], provides: [], declarations: [],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
