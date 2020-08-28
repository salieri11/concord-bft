/*
 * Copyright 2018-2020 VMware, all rights reserved.
 */

import { testFor, prepareEach, beforeTesting } from '../../../test.helper.spec';

import { DetailsComponent } from './details.component';
import { ClarityModule } from '@clr/angular';
import { CanViewDirective } from '../../shared/directives/can-view.directive';
import { FeatureFlagDirective } from '../../shared/directives/feature-flag.directive';

describe('DetailsComponent', () => {
  const test = testFor(DetailsComponent).expedite({
    imports: [ClarityModule], provides: [], declarations: [DetailsComponent, CanViewDirective, FeatureFlagDirective],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
