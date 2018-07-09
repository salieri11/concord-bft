/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Routes } from '@angular/router';

import { NodesComponent } from './nodes/nodes.component';
import { NodeComponent } from './node/node.component';

export const nodeRoutes: Routes = [
  { path: '', component: NodesComponent },
  { path: ':id', component: NodeComponent }
];

