/*
 * Copyright 2020 VMware, all rights reserved.
 */
import { WavefrontGraphComponent } from './wavefront-graph.component';
import { testFor, beforeTesting, prepareEach } from '../../../test.helper.spec';

describe('WavefrontGraphComponent', () => {
  const test = testFor(WavefrontGraphComponent).expedite({
    imports: [], provides: [], declarations: [WavefrontGraphComponent],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
