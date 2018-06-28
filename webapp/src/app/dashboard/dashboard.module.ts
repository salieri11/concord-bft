/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { SharedModule } from '../shared/shared.module';

import { AuthenticatedGuard } from '../shared/authenticated-guard.service';

import { TransactionsModule } from '../transactions/transactions.module';
import { DashboardComponent } from './dashboard-container/dashboard.component';

const routes: Routes = [
  {
    path: 'dashboard',
    canActivateChild: [AuthenticatedGuard],
    children: [
      {path: '', component: DashboardComponent}
    ]
  }
];

@NgModule({
  imports: [
    SharedModule,
    TransactionsModule,
    RouterModule.forChild(routes)
  ],
  declarations: [DashboardComponent]
})
export class DashboardModule { }

