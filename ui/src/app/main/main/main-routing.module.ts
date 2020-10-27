/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

import { MainComponent } from './main.component';
import { AuthenticatedGuard } from '../../shared/authenticated-guard.service';
import { AgreementGuard } from '../../shared/agreement-guard.service';
import { BlockchainResolver } from '../../blockchain/shared/blockchain.resolver';
import { ZonesComponent } from '../../zones/zones.component';
import { DeployComponent } from '../deploy/deploy.component';
import { WelcomeComponent } from '../welcome/welcome.component';
import { DeployingComponent } from '../deploying/deploying.component';

import { mainRoutes } from './../../shared/urls.model';
import { zonesRoutes } from '../../zones/zones-routing';
import { dashboardRoutes } from '../../dashboard/dashboard-routing';
import { nodeRoutes } from '../../nodes/nodes-routing';
import { consortiumRoutes } from '../../consortium/consortium-routing';
import { orgRoutes } from '../../orgs/orgs-routing';
// import { usersRoutes } from '../../users/users-routing';
// import { loggingRoutes } from '../../logging/logging-routing';
// import { developerRoutes } from '../../developer/developer-routing';
import { detailsRoutes } from '../../details/details-routing';
import { blockRoutes } from '../../blocks/blocks-routing';
import { transactionsRoutes } from '../../transactions/transactions-routing';


const routes: Routes = [
  // ROUTE: /:blockchainId
  {
    path: ':blockchainId',
    component: MainComponent,
    canActivate: [AuthenticatedGuard, AgreementGuard],
    canActivateChild: [AuthenticatedGuard],
    resolve: { blockchain: BlockchainResolver },
    runGuardsAndResolvers: 'pathParamsChange',
    children: [{
      path: mainRoutes.welcome, component: WelcomeComponent
    }, {
      path: mainRoutes.deploy, component: DeployComponent
    }, {
      path: mainRoutes.deploying, children: [{
        path: ':taskId', component: DeployingComponent
      }]
    },
    {
      path: mainRoutes.zones,
      children: zonesRoutes,
      component: ZonesComponent
    },
    {
      path: mainRoutes.dashboard,
      children: dashboardRoutes
    },
    {
      path: mainRoutes.blocks,
      children: blockRoutes
    },
    {
      path: mainRoutes.nodes,
      children: nodeRoutes
    },
    {
      path: mainRoutes.smartContracts,
      loadChildren: () => import('./../../smart-contracts/smart-contracts.module').then(m => m.SmartContractsModule)
    },
    {
      path: mainRoutes.logging,
      loadChildren: () => import('./../../logging/logging.module').then(m => m.LoggingModule)

    },
    {
      path: mainRoutes.consortiums,
      children: consortiumRoutes
    },
    {
      path: mainRoutes.organizations,
      children: orgRoutes
    },
    {
      path: mainRoutes.transactions,
      children: transactionsRoutes
    },
    {
      path: mainRoutes.developer,
      loadChildren: () => import('./../../developer/developer.module').then(m => m.DeveloperModule)
    },
    {
      path: mainRoutes.details,
      children: detailsRoutes
    }]
  }
];


@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class MainRoutingModule {
}
