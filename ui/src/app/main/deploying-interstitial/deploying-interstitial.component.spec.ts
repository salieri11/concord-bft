/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { testFor, prepareEach, beforeTesting } from '../../../test.helper.spec';
import { DeployingInterstitialComponent } from './deploying-interstitial.component';

describe('DeployingInterstitialComponent', () => {
  const test = testFor(DeployingInterstitialComponent).expedite({
    imports: [], provides: [], declarations: [DeployingInterstitialComponent],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
