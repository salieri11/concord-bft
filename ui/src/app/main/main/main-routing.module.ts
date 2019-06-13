/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

import { environment } from './../../../environments/environment';
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
import { loggingRoutes } from '../../logging/logging-routing';
import { developerRoutes } from '../../developer/developer-routing';


const guards = [];

if (environment.csp) {
  guards.push(AuthenticatedGuard);
} else {
  guards.push(AgreementGuard);
}
const routes: Routes = [{
    path: ':consortiumId',
    component: MainComponent,
    canActivate: guards,
    canActivateChild: [AuthenticatedGuard],
    children: [
      { path: 'dashboard', children: dashboardRoutes },
      { path: 'blocks', children: blockRoutes },
      { path: 'nodes', children: nodeRoutes },
      { path: 'smart-contracts', children: smartContractRoutes },
      { path: 'logging', children: loggingRoutes },
      { path: 'consortium', children: consortiumRoutes },
      { path: 'organization', children: orgRoutes },
      { path: 'users', children: usersRoutes },
      { path: 'transactions', children: transactionsRoutes },
      { path: 'developer', children: developerRoutes },
    ]
  }];

if (environment.csp) {
  routes.push({ path: '', component: MainComponent, canActivate: guards});
}


@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class MainRoutingModule {
}
