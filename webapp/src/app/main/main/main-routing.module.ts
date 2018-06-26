/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

import { MainComponent } from './main.component';
import { dashboardRoutes } from '../../dashboard/dashboard-routing';
import { AuthenticatedGuard } from '../../shared/authenticated-guard.service';
import { blockRoutes } from '../../blocks/blocks-routing';
import { nodeRoutes } from '../../nodes/nodes-routing';
import { testingRoutes } from '../../testing/testing-routing';
import { blockChainsRoutes } from '../../blockchains/blockchains-routing';
import { consortiumManagementRoutes } from '../../consortium-management/consortium-management-routing';
import { kubernetesManagementRoutes } from '../../kubernetes-management/kubernetes-management-routing';
import { OrganizationRoutes } from '../../org-management/org-routing';
import { smartContractRoutes } from "../../smart-contracts/smart-contracts-routing";

const routes: Routes = [
  {
    path: '',
    component: MainComponent,
    canActivateChild: [AuthenticatedGuard],
    children: [
      { path: '', redirectTo: 'dashboard', pathMatch: 'full' },
      { path: 'dashboard', children: dashboardRoutes },
      { path: 'blocks', children: blockRoutes },
      { path: 'nodes', children: nodeRoutes },
      { path: 'smart-contracts', children: smartContractRoutes },
      { path: 'testing', children: testingRoutes },
      { path: 'blockchains', children: blockChainsRoutes },
      { path: 'consortium', children: consortiumManagementRoutes },
      { path: 'organization', children: OrganizationRoutes },
      { path: 'kubernetes', children: kubernetesManagementRoutes }
    ]
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class MainRoutingModule {
}
