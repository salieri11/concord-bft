/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { TransactionDetailContainerComponent } from './transaction-detail-container/transaction-detail-container.component';
import { RouterModule, Routes } from '@angular/router';
import { TranslateModule } from '@ngx-translate/core';
import { ClarityModule } from '@clr/angular';
import { SharedModule } from '../shared/shared.module';

const routes: Routes = [
  {path: 'blocks/:blockNumber/transactions/:transactionHash', component: TransactionDetailContainerComponent},
];

@NgModule({
  imports: [
    CommonModule,
    RouterModule.forChild(routes),
    ClarityModule,
    TranslateModule,
    SharedModule
  ],
  declarations: [TransactionDetailContainerComponent]
})
export class TransactionsModule { }
