/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { DeployingComponent } from './deploying.component';
import { testFor, prepareEach, beforeTesting } from '../../../test.helper.spec';

describe('DeployingComponent', () => {
  const test = testFor(DeployingComponent).expedite({
    imports: [], provides: [], declarations: [DeployingComponent],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
