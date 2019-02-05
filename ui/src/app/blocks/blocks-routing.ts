/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Routes } from '@angular/router';

import { BlockListComponent } from './block-list/block-list.component';
import { BlockComponent } from './block/block.component';
import { TransactionComponent } from '../transactions/transaction/transaction.component';



export const blockRoutes: Routes = [
  { path: '', component: BlockListComponent },
  {
    path: ':blockNumber',
    component: BlockComponent,
  },
  {
    path: ':blockNumber/transactions/:transactionHash', component: TransactionComponent
  }
];
