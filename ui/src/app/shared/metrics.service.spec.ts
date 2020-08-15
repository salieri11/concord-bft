/*
 * Copyright 2018-2020 VMware, all rights reserved.
 */
import { MetricsService } from './metrics.service';
import { testFor, prepareEach, beforeTesting } from '../../test.helper.spec';

describe('MetricsService', () => {
  const test = testFor(MetricsService).expedite({
    imports: [], provides: [], declarations: [],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
