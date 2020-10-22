/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule } from '@angular/router';
import { TourNgxPopperModule } from 'ngx-tour-ngx-popper';
import { NgxChartsModule } from '@swimlane/ngx-charts';

import { SharedModule } from '../shared/shared.module';
import { TransactionsModule } from '../transactions/transactions.module';
import { NodesModule } from '../nodes/nodes.module';
import { DashboardComponent } from './dashboard/dashboard.component';
import { GraphsModule } from '../graphs/graphs.module';
import { BlocksModule } from '../blocks/blocks.module';
import { DashboardListComponent } from './dashboard-list/dashboard-list.component';
import { DetailsModule } from '../details/details.module';

@NgModule({
  imports: [
    SharedModule,
    RouterModule,
    TransactionsModule,
    NodesModule,
    BlocksModule,
    GraphsModule,
    TourNgxPopperModule,
    NgxChartsModule,
    DetailsModule,
  ],
  declarations: [
    DashboardComponent,
    DashboardListComponent,
  ]
})
export class DashboardModule { }
