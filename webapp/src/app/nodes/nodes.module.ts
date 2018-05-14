/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

import { SharedModule } from '../shared/shared.module';

import { AuthenticatedGuard } from '../shared/authenticated-guard.service';

import { NodesContainerComponent } from './nodes-container/nodes-container.component';
import { NodeDetailContainerComponent } from './node-detail-container/node-detail-container.component';
import { NodesStatusFilterComponent } from './nodes-status-filter/nodes-status-filter.component';

const routes: Routes = [
  {
    path: 'nodes',
    canActivateChild: [AuthenticatedGuard],
    children: [
      {path: '', component: NodesContainerComponent},
      {path: ':id', component: NodeDetailContainerComponent}
    ]
  }
];

@NgModule({
  imports: [
    RouterModule.forChild(routes),
    SharedModule
  ],
  declarations: [NodesContainerComponent, NodeDetailContainerComponent, NodesStatusFilterComponent]
})
export class NodesModule { }
