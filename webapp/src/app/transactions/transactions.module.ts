/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { TranslateModule } from '@ngx-translate/core';

import { ClarityModule } from '@clr/angular';

import { SharedModule } from '../shared/shared.module';
import { TransactionComponent } from './transaction/transaction.component';
import { TransactionsListComponent } from './transactions-list/transactions-list.component';
import { TransactionDetailsComponent } from './transaction-details/transaction-details.component';


@NgModule({
  imports: [
    CommonModule,
    RouterModule,
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
