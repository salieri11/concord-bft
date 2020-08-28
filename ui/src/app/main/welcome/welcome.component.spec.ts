/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { WelcomeComponent } from './welcome.component';
import { testFor, prepareEach, beforeTesting } from '../../../test.helper.spec';

describe('WelcomeComponent', () => {
  const test = testFor(WelcomeComponent).expedite({
    imports: [], provides: [], declarations: [WelcomeComponent],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
