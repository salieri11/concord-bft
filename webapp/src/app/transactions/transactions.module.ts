/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { TranslateModule } from '@ngx-translate/core';

import { ClarityModule } from '@clr/angular';

import { SharedModule } from '../shared/shared.module';
import { TransactionDetailContainerComponent } from './transaction-detail-container/transaction-detail-container.component';
import { TransactionListViewComponent } from './transaction-list-view/transaction-list-view.component';
import { TransactionDetailsComponent } from './transaction-details/transaction-details.component';


@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    ClarityModule,
    TranslateModule,
    SharedModule
  ],
  declarations: [TransactionDetailContainerComponent, TransactionListViewComponent, TransactionDetailsComponent],
  exports: [
    TransactionListViewComponent,
    TransactionDetailsComponent,
    TransactionDetailContainerComponent
  ]

})
export class TransactionsModule { }
