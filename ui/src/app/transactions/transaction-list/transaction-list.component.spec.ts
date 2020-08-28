/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { testFor, prepareEach, beforeTesting } from '../../../test.helper.spec';
import { TransactionListComponent } from './transaction-list.component';
import { ClarityModule } from '@clr/angular';
import { TransactionsStatusFilterComponent } from '../../shared/components/transactions-status-filter/transactions-status-filter.component';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';

describe('TransactionListComponent', () => {
  const test = testFor(TransactionListComponent).expedite({
    imports: [ClarityModule, BrowserAnimationsModule], provides: [],
    declarations: [TransactionListComponent, TransactionsStatusFilterComponent],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
