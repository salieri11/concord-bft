/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { testFor, prepareEach, beforeTesting } from '../../../test.helper.spec';
import { DeployComponent } from './deploy.component';
import { BlockchainWizardComponent } from '../../blockchain/blockchain-wizard/blockchain-wizard.component';
import { FeatureFlagDirective } from '../../shared/directives/feature-flag.directive';
import { ClarityModule } from '@clr/angular';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';

describe('DeployComponent', () => {
  const test = testFor(DeployComponent).expedite({
    imports: [ClarityModule, BrowserAnimationsModule],
    provides: [],
    declarations: [DeployComponent, BlockchainWizardComponent, FeatureFlagDirective],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
