/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule } from '@angular/router';

import { SharedModule } from '../shared/shared.module';
import { GraphsModule } from '../graphs/graphs.module';
import { TransactionsModule } from '../transactions/transactions.module';
import { NodeComponent } from './node/node.component';
import { NodesComponent } from './nodes/nodes.component';
import { NodesStatusFilterComponent } from './nodes-status-filter/nodes-status-filter.component';
import { NodeListComponent } from './node-list/node-list.component';

@NgModule({
  imports: [
    TransactionsModule,
    RouterModule,
    SharedModule,
    GraphsModule
  ],
  declarations: [NodesComponent, NodeComponent, NodesStatusFilterComponent, NodeListComponent]
})
export class NodesModule { }
