/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule } from '@angular/router';

import { SharedModule } from '../shared/shared.module';
import { NodesStatusFilterComponent } from './nodes-status-filter/nodes-status-filter.component';
import { TransactionsModule } from '../transactions/transactions.module';
import { NodeComponent } from './node/node.component';
import { NodesComponent } from './nodes/nodes.component';



@NgModule({
  imports: [
    TransactionsModule,
    RouterModule,
    SharedModule
  ],
  declarations: [NodesComponent, NodeComponent, NodesStatusFilterComponent]
})
export class NodesModule { }
