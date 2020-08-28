/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { testFor, beforeTesting, prepareEach } from '../../../test.helper.spec';
import { ApiComponent } from './api.component';

describe('ApiComponent', () => {
  const test = testFor(ApiComponent).expedite({
    imports: [], provides: [], declarations: [ApiComponent],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
