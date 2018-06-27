/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';

import { AuthenticatedGuard } from '../shared/authenticated-guard.service';
import { ConsortiumManagementComponent } from './consortium-management.component';

const routes: Routes = [
  {
    path: 'consortium',
    canActivate: [AuthenticatedGuard],
    component: ConsortiumManagementComponent,
  },

];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class ConsortiumRoutingModule { }
