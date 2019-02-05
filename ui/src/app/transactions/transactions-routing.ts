/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Routes } from '@angular/router';

import { TransactionListComponent } from './transaction-list/transaction-list.component';
import { TransactionComponent } from './transaction/transaction.component';

export const transactionsRoutes: Routes = [
  { path: '', component: TransactionListComponent },
  {
    path: ':transactionHash', component: TransactionComponent
  }
];
