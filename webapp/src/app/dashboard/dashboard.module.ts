/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { SharedModule } from '../shared/shared.module';

import { AuthenticatedGuard } from '../shared/authenticated-guard.service';

import { DashboardContainerComponent } from './dashboard-container/dashboard-container.component';
import { TransactionsModule } from "../transactions/transactions.module";

const routes: Routes = [
  {
    path: 'dashboard',
    canActivateChild: [AuthenticatedGuard],
    children: [
      {path: '', component: DashboardContainerComponent}
    ]
  }
];

@NgModule({
  imports: [
    SharedModule,
    TransactionsModule,
    RouterModule.forChild(routes)
  ],
  declarations: [DashboardContainerComponent]
})
export class DashboardModule { }

