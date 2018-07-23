/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Routes } from '@angular/router';

import { TransactionComponent } from './transaction/transaction.component';

export const transactionsRoutes: Routes = [
  {
    path: ':transactionHash', component: TransactionComponent
  }
];
