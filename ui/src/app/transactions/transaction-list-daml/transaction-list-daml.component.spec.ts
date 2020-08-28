/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { TransactionListDamlComponent } from './transaction-list-daml.component';
import { testFor, prepareEach, beforeTesting } from '../../../test.helper.spec';
import { ClarityModule } from '@clr/angular';

describe('TransactionListDamlComponent', () => {
  const test = testFor(TransactionListDamlComponent).expedite({
    imports: [ClarityModule], provides: [], declarations: [TransactionListDamlComponent],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
