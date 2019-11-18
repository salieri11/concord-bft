/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { ClarityModule } from '@clr/angular';
import { TranslateModule } from '@ngx-translate/core';
import { TourNgxPopperModule } from 'ngx-tour-ngx-popper';

import { SharedModule } from '../shared/shared.module';
import { TransactionComponent } from './transaction/transaction.component';
import { TransactionListComponent } from './transaction-list/transaction-list.component';
import { TransactionDetailsComponent } from './transaction-details/transaction-details.component';


@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    ClarityModule,
    TranslateModule,
    SharedModule,
    TourNgxPopperModule
  ],
  declarations: [
    TransactionComponent,
    TransactionListComponent,
    TransactionDetailsComponent,
  ],
  exports: [
    TransactionListComponent,
    TransactionDetailsComponent
  ]

})
export class TransactionsModule { }
