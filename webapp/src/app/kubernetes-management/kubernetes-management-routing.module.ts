/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';

import { KubernetesManagementComponent } from './kubernetes-management.component';
import { AuthenticatedGuard } from '../shared/authenticated-guard.service';

const routes: Routes = [
  {
    path: 'kubernetes',
    canActivate: [AuthenticatedGuard],
    component: KubernetesManagementComponent,
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class KubernetesRoutingModule { }
