/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';

import { SharedModule } from '../shared/shared.module';
import { DashboardContainerComponent } from './dashboard-container/dashboard-container.component';
import { TransactionsModule } from '../transactions/transactions.module';

@NgModule({
  imports: [
    SharedModule,
    TransactionsModule,
  ],
  declarations: [DashboardContainerComponent]
})
export class DashboardModule { }

