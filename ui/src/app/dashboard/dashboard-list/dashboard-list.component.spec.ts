/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { DashboardListComponent } from './dashboard-list.component';
import { testFor, prepareEach, beforeTesting } from '../../../test.helper.spec';
import { ClarityModule } from '@clr/angular';


describe('DashboardListComponent', () => {
  const test = testFor(DashboardListComponent).expedite({
    imports: [ClarityModule], provides: [], declarations: [DashboardListComponent],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
