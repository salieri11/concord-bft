/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule } from '@angular/router';

import { NgxChartsModule } from '@swimlane/ngx-charts';

import { SharedModule } from '../shared/shared.module';
import { GraphsModule } from '../graphs/graphs.module';
import { TransactionsModule } from '../transactions/transactions.module';
import { NodesComponent } from './nodes/nodes.component';
import { NodesStatusFilterComponent } from './nodes-status-filter/nodes-status-filter.component';
import { NodeListComponent } from './node-list/node-list.component';
import { DeployClientComponent } from './deploy-client/deploy-client.component';
import { NodeDashboardComponent } from './node-dashboard/node-dashboard.component';

@NgModule({
  imports: [
    TransactionsModule,
    RouterModule,
    SharedModule,
    NgxChartsModule,
    GraphsModule,
  ],
  declarations: [
    NodesComponent,
    NodesStatusFilterComponent,
    NodeListComponent,
    DeployClientComponent,
    NodeDashboardComponent
  ],
  exports: [
    NodeListComponent, NodeDashboardComponent
  ]
})
export class NodesModule { }
