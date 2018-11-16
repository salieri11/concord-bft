/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

import { MainComponent } from './main.component';
import { dashboardRoutes } from '../../dashboard/dashboard-routing';
import { AuthenticatedGuard } from '../../shared/authenticated-guard.service';
import { AgreementGuard } from '../../shared/agreement-guard.service';
import { blockRoutes } from '../../blocks/blocks-routing';
import { nodeRoutes } from '../../nodes/nodes-routing';
import { smartContractRoutes } from '../../smart-contracts/smart-contracts-routing';
import { consortiumRoutes } from '../../consortium/consortium-routing';
import { orgRoutes } from '../../orgs/orgs-routing';
import { usersRoutes } from '../../users/users-routing';
import { transactionsRoutes } from '../../transactions/transactions-routing';

const routes: Routes = [
  {
    path: '',
    component: MainComponent,
    canActivate: [AgreementGuard],
    canActivateChild: [AuthenticatedGuard],
    children: [
      { path: 'dashboard', children: dashboardRoutes },
      { path: 'blocks', children: blockRoutes },
      { path: 'nodes', children: nodeRoutes },
      { path: 'smart-contracts', children: smartContractRoutes },
      { path: 'consortium', children: consortiumRoutes },
      { path: 'organization', children: orgRoutes },
      { path: 'users', children: usersRoutes },
      { path: 'transactions', children: transactionsRoutes },
    ]
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class MainRoutingModule {
}
