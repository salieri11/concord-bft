/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { testFor, beforeTesting, prepareEach } from '../../../test.helper.spec';
import { DashboardComponent } from './dashboard.component';


describe('DashboardComponent', () => {
  const test = testFor(DashboardComponent).expedite({
    imports: [], provides: [], declarations: [DashboardComponent],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
