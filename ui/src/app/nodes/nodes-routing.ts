/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Routes } from '@angular/router';

import { NodesComponent } from './nodes/nodes.component';

export const nodeRoutes: Routes = [
  {
    path: ':nodeTypeOrId', component: NodesComponent
  },
  {
    path: '', redirectTo: 'committers', pathMatch: 'full'
  }
];

