/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { SignUpComponent } from './sign-up.component';
import { prepareEach, beforeTesting, testFor } from '../../../test.helper.spec';

describe('SignUpComponent', () => {
  const test = testFor(SignUpComponent).expedite({
    imports: [], provides: [], declarations: [SignUpComponent],
  }, beforeTesting(() => {}), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
