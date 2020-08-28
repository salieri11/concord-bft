/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { WelcomeContentComponent } from './welcome-content.component';
import { testFor, prepareEach, beforeTesting } from '../../../test.helper.spec';

describe('WelcomeContentComponent', () => {
  const test = testFor(WelcomeContentComponent).expedite({
    imports: [], provides: [], declarations: [WelcomeContentComponent],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
