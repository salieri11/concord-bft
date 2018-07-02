/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Routes } from '@angular/router';

import { BlockDetailContainerComponent } from './block-detail-container/block-detail-container.component';
import { BlocksContainerComponent } from './blocks-container/blocks-container.component';
import { TransactionDetailContainerComponent } from '../transactions/transaction-detail-container/transaction-detail-container.component';

export const blockRoutes: Routes = [
  { path: '', component: BlocksContainerComponent },
  {
    path: ':blockNumber',
    component: BlockDetailContainerComponent,
  },
  {
    path: ':blockNumber/transactions/:transactionHash', component: TransactionDetailContainerComponent
  }
];
