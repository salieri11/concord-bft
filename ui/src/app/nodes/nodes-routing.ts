/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Routes } from '@angular/router';

import { NodesComponent } from './nodes/nodes.component';
import { AuthenticatedGuard } from '../shared/authenticated-guard.service';

export const nodeRoutes: Routes = [
  {
    path: ':nodeTypeOrId', component: NodesComponent,
    canActivate: [AuthenticatedGuard]
  },
  {
    path: '', redirectTo: 'replicas', pathMatch: 'full'
  }
];

