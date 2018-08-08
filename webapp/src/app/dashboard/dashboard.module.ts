/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule } from '@angular/router';
import { TourNgxPopperModule } from 'ngx-tour-ngx-popper';

import { SharedModule } from '../shared/shared.module';
import { TransactionsModule } from '../transactions/transactions.module';
import { DashboardComponent } from './dashboard-container/dashboard.component';
import { BlockGraphModule } from '../block-graph/block-graph.module';

@NgModule({
  imports: [
    SharedModule,
    RouterModule,
    TransactionsModule,
    BlockGraphModule,
    TourNgxPopperModule
  ],
  declarations: [DashboardComponent]
})
export class DashboardModule { }

