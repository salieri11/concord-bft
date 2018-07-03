/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';

import { SharedModule } from '../shared/shared.module';
import { TransactionsModule } from '../transactions/transactions.module';
import { DashboardComponent } from './dashboard-container/dashboard.component';

@NgModule({
  imports: [
    SharedModule,
    TransactionsModule,
  ],
  declarations: [DashboardComponent]
})
export class DashboardModule { }

