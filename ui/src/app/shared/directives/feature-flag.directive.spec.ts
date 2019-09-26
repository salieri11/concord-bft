/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { FeatureFlagDirective } from './feature-flag.directive';

describe('Directive: FeatureFlag', () => {
  it('should create an instance', () => {
    const directive = new FeatureFlagDirective(null, null, null);
    expect(directive).toBeTruthy();
  });
});
