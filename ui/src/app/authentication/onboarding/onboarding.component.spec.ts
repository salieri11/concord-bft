/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { OnboardingComponent } from './onboarding.component';
import { testFor, prepareEach, beforeTesting } from '../../../test.helper.spec';

describe('OnboardingComponent', () => {
  const test = testFor(OnboardingComponent).expedite({
    imports: [], provides: [], declarations: [OnboardingComponent],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
