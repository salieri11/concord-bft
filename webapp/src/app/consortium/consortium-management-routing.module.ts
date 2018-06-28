/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';

import { AuthenticatedGuard } from '../shared/authenticated-guard.service';
import { ConsortiumComponent } from './consortium.component';

const routes: Routes = [
  {
    path: 'consortium',
    canActivate: [AuthenticatedGuard],
    component: ConsortiumComponent,
  },

];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class ConsortiumRoutingModule { }
