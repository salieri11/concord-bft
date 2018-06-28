/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule, Routes } from '@angular/router';
import { TranslateModule } from '@ngx-translate/core';

import { ClarityModule } from '@clr/angular';

import { SharedModule } from '../shared/shared.module';
import { AuthenticatedGuard } from '../shared/authenticated-guard.service';
import { TransactionComponent } from './transaction/transaction.component';
import { TransactionsListComponent } from './transactions-list/transactions-list.component';
import { TransactionDetailsComponent } from './transaction-details/transaction-details.component';

const routes: Routes = [
  {
    path: 'blocks/:blockNumber/transactions',
    canActivateChild: [AuthenticatedGuard],
    children: [
      {path: ':transactionHash', component: TransactionComponent},
    ]
  },
  {
    path: 'transactions/:transactionHash', component: TransactionComponent
  }
];

@NgModule({
  imports: [
    CommonModule,
    RouterModule.forChild(routes),
    ClarityModule,
    TranslateModule,
    SharedModule
  ],
  declarations: [TransactionComponent, TransactionsListComponent, TransactionDetailsComponent],
  exports: [
    TransactionsListComponent,
    TransactionDetailsComponent
  ]

})
export class TransactionsModule { }
