/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { testFor, prepareEach, beforeTesting } from '../../../../test.helper.spec';
import { VersionComponent } from './version.component';

describe('VersionComponent', () => {
  const test = testFor(VersionComponent).expedite({
    imports: [], provides: [], declarations: [VersionComponent],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
