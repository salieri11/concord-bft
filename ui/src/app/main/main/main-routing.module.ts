/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

import { MainComponent } from './main.component';
import { AuthenticatedGuard } from '../../shared/authenticated-guard.service';
import { AgreementGuard } from '../../shared/agreement-guard.service';
import { BlockchainResolver } from '../../blockchain/shared/blockchain.service';
import { ZonesComponent } from '../../zones/zones.component';
import { DeployComponent } from '../deploy/deploy.component';
import { WelcomeComponent } from '../welcome/welcome.component';
import { DeployingComponent } from '../deploying/deploying.component';

import { mainRoutes } from './../../shared/urls.model';
import { zonesRoutes } from '../../zones/zones-routing';
import { dashboardRoutes } from '../../dashboard/dashboard-routing';
import { transactionsRoutes } from '../../transactions/transactions-routing';
import { blockRoutes } from '../../blocks/blocks-routing';
import { nodeRoutes } from '../../nodes/nodes-routing';
import { consortiumRoutes } from '../../consortium/consortium-routing';
import { smartContractRoutes } from '../../smart-contracts/smart-contracts-routing';
import { orgRoutes } from '../../orgs/orgs-routing';
import { usersRoutes } from '../../users/users-routing';
import { loggingRoutes } from '../../logging/logging-routing';
import { developerRoutes } from '../../developer/developer-routing';
import { detailsRoutes } from '../../details/details-routing';


const routes: Routes = [
  // ROUTE: /:consortiumId
  {
    path: ':consortiumId',
    component: MainComponent,
    canActivate: [AuthenticatedGuard, AgreementGuard],
    canActivateChild: [AuthenticatedGuard],
    resolve: {blockchain: BlockchainResolver},
    children: [{
        // ROUTE: /blockchain/welcome
        path: mainRoutes.welcome, component: WelcomeComponent
      }, {
        // ROUTE: /blockchain/deploy
        path: mainRoutes.deploy, component: DeployComponent
      }, {
        // ROUTE: /blockchain/deploying/*
        path: mainRoutes.deploying, children: [{
          // ROUTE: /blockchain/deploying/:taskId
          path: ':taskId', component: DeployingComponent
        }]
      },
      // ROUTE: /:consortiumId/zones
      {
        component: ZonesComponent,
        path: mainRoutes.zones,
        children: zonesRoutes
      },
      // ROUTE: /:consortiumId/dashboard
      { path: mainRoutes.dashboard, children: dashboardRoutes },
      // ROUTE: /:consortiumId/blocks
      { path: mainRoutes.blocks, children: blockRoutes },
      // ROUTE: /:consortiumId/nodes
      { path: mainRoutes.nodes, children: nodeRoutes },
      // ROUTE: /:consortiumId/smart-contracts
      { path: mainRoutes.smartContracts, children: smartContractRoutes },
      // ROUTE: /:consortiumId/logging
      { path: mainRoutes.logging, children: loggingRoutes },
      // ROUTE: /:consortiumId/consortiums
      { path: mainRoutes.consortiums, children: consortiumRoutes },
      // ROUTE: /:consortiumId/organizations
      { path: mainRoutes.organizations, children: orgRoutes },
      // ROUTE: /:consortiumId/users
      { path: mainRoutes.system, children: usersRoutes },
      // ROUTE: /:consortiumId/transactions
      { path: mainRoutes.transactions, children: transactionsRoutes },
      // ROUTE: /:consortiumId/developer
      { path: mainRoutes.developer, children: developerRoutes },
      // ROUTE: /:consortiumId/details
      { path: mainRoutes.details, children: detailsRoutes },
    ]
  },
];


@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class MainRoutingModule {
}
