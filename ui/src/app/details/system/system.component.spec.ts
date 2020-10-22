/*
 * Copyright 2018-2020 VMware, all rights reserved.
 */

import { testFor, prepareEach, beforeTesting } from '../../../test.helper.spec';

import { ClarityModule } from '@clr/angular';
import { SystemComponent } from './system.component';

describe('SystemComponent', () => {
  const test = testFor(SystemComponent).expedite({
    imports: [ClarityModule],
    provides: [],
    declarations: [SystemComponent],
  }, beforeTesting(() => { }), prepareEach(() => { }));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
