/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule, Routes } from '@angular/router';
import { FormsModule } from '@angular/forms';
import { ClarityModule } from '@clr/angular';
import { TranslateModule } from '@ngx-translate/core';
import { SharedModule } from '../shared/shared.module';

import { AuthenticatedGuard } from '../shared/authenticated-guard.service';

import { NodesContainerComponent } from './nodes-container/nodes-container.component';
import { NodeDetailContainerComponent } from './node-detail-container/node-detail-container.component';
import { TransactionFiltersModalComponent } from './transaction-filters-modal/transaction-filters-modal.component';
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
    CommonModule,
    RouterModule.forChild(routes),
    ClarityModule,
    TranslateModule,
    FormsModule,
    SharedModule
  ],
  declarations: [NodesContainerComponent, NodeDetailContainerComponent, TransactionFiltersModalComponent, NodesStatusFilterComponent]
})
export class NodesModule { }
