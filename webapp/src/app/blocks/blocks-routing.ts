/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Routes } from '@angular/router';

import { BlocksListComponent } from './blocks-list/blocks-list.component';
import { BlockComponent } from './block/block.component';
import { TransactionComponent } from '../transactions/transaction/transaction.component';



export const blockRoutes: Routes = [
  { path: '', component: BlocksListComponent },
  {
    path: ':blockNumber',
    component: BlockComponent,
  },
  {
    path: ':blockNumber/transactions/:transactionHash', component: TransactionComponent
  }
];
