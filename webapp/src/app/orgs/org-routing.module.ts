/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';

import { AuthenticatedGuard } from '../shared/authenticated-guard.service';
import { OrgsComponent } from './orgs.component';

const routes: Routes = [
  {
    path: 'organization',
    canActivate: [AuthenticatedGuard],
    component: OrgsComponent
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class OrgRoutingModule { }
