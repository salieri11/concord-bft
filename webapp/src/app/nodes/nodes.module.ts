/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

import { SharedModule } from '../shared/shared.module';

import { AuthenticatedGuard } from '../shared/authenticated-guard.service';

import { NodeComponent } from './node/node.component';
import { NodesStatusFilterComponent } from './nodes-status-filter/nodes-status-filter.component';
import { TransactionsModule } from '../transactions/transactions.module';
import { NodesComponent } from './nodes/nodes.component';

const routes: Routes = [
  {
    path: 'nodes',
    canActivateChild: [AuthenticatedGuard],
    children: [
      { path: '', component: NodesComponent },
      { path: ':id', component: NodeComponent }
    ]
  }
];

@NgModule({
  imports: [
    RouterModule.forChild(routes),
    TransactionsModule,
    SharedModule
  ],
  declarations: [NodesComponent, NodeComponent, NodesStatusFilterComponent]
})
export class NodesModule { }
