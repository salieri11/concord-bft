/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { testFor, prepareEach, beforeTesting } from '../../../test.helper.spec';
import { NodeDashboardComponent } from './node-dashboard.component';
import { ClarityModule } from '@clr/angular';

describe('NodeDashboardComponent', () => {
  const test = testFor(NodeDashboardComponent).expedite({
    imports: [ClarityModule], provides: [], declarations: [NodeDashboardComponent],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
