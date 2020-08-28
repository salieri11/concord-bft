/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { testFor, prepareEach, beforeTesting } from '../../../test.helper.spec';
import { TransactionComponent } from './transaction.component';

describe('TransactionComponent', () => {
  const test = testFor(TransactionComponent).expedite({
    imports: [], provides: [], declarations: [TransactionComponent],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
