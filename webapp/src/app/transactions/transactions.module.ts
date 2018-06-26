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
import { TransactionDetailContainerComponent } from './transaction-detail-container/transaction-detail-container.component';
import { TransactionListViewComponent } from './transaction-list-view/transaction-list-view.component';
import { TransactionDetailsComponent } from './transaction-details/transaction-details.component';

const routes: Routes = [
  {
    path: 'blocks/:blockNumber/transactions',
    canActivateChild: [AuthenticatedGuard],
    children: [
      {path: ':transactionHash', component: TransactionDetailContainerComponent},
    ]
  },
  {
    path: 'transactions/:transactionHash', component: TransactionDetailContainerComponent
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
  declarations: [TransactionDetailContainerComponent, TransactionListViewComponent, TransactionDetailsComponent],
  exports: [
    TransactionListViewComponent,
    TransactionDetailsComponent
  ]

})
export class TransactionsModule { }
