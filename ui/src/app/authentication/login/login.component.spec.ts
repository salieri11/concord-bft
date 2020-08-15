/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { LogInContainerComponent } from './login.component';
import { testFor, beforeTesting, prepareEach } from '../../../test.helper.spec';

describe('LogInContainerComponent', () => {
  const test = testFor(LogInContainerComponent).expedite({
    imports: [], provides: [], declarations: [LogInContainerComponent],
  }, beforeTesting(() => {}), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
