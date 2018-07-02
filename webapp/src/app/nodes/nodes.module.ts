/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule } from '@angular/router';

import { SharedModule } from '../shared/shared.module';
import { NodesContainerComponent } from './nodes-container/nodes-container.component';
import { NodeDetailContainerComponent } from './node-detail-container/node-detail-container.component';
import { NodesStatusFilterComponent } from './nodes-status-filter/nodes-status-filter.component';
import { TransactionsModule } from '../transactions/transactions.module';



@NgModule({
  imports: [
    TransactionsModule,
    RouterModule,
    SharedModule
  ],
  declarations: [NodesContainerComponent, NodeDetailContainerComponent, NodesStatusFilterComponent]
})
export class NodesModule { }
