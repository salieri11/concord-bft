/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';

import { AuthenticatedGuard } from '../shared/authenticated-guard.service';
import { KubernetesComponent } from './kubernetes.component';

const routes: Routes = [
  {
    path: 'kubernetes',
    canActivate: [AuthenticatedGuard],
    component: KubernetesComponent,
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class KubernetesRoutingModule { }
