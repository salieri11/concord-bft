/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

import { MainComponent } from './main.component';
import { ForbiddenComponent } from './../../errors/forbidden/forbidden.component';
import { ErrorComponent } from './../../errors/error/error.component';
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
import { BlockchainResolver } from '../../blockchain/shared/blockchain.service';
import { mainRoutes } from './../../shared/urls.model';


const routes: Routes = [{
    path: mainRoutes.forbidden,
    component: ForbiddenComponent,
    canActivate: [AuthenticatedGuard],
  }, {
    path: mainRoutes.error,
    component: ErrorComponent,
    canActivate: [AuthenticatedGuard],
  }, {
    path: ':consortiumId',
    component: MainComponent,
    canActivate: [AuthenticatedGuard, AgreementGuard],
    canActivateChild: [AuthenticatedGuard],
    resolve: {blockchain: BlockchainResolver},
    children: [
      { path: mainRoutes.dashboard, children: dashboardRoutes },
      { path: mainRoutes.blocks, children: blockRoutes },
      { path: mainRoutes.nodes, children: nodeRoutes },
      { path: mainRoutes.smartContracts, children: smartContractRoutes },
      { path: mainRoutes.logging, children: loggingRoutes },
      { path: mainRoutes.consortiums, children: consortiumRoutes },
      { path: mainRoutes.organizations, children: orgRoutes },
      { path: mainRoutes.users, children: usersRoutes },
      { path: mainRoutes.transactions, children: transactionsRoutes },
      { path: mainRoutes.developer, children: developerRoutes },
    ]
  }];


@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class MainRoutingModule {
}
