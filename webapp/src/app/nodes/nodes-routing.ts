/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Routes } from '@angular/router';

import { NodesContainerComponent } from './nodes-container/nodes-container.component';
import { NodeDetailContainerComponent } from './node-detail-container/node-detail-container.component';

export const nodeRoutes: Routes = [
  { path: '', component: NodesContainerComponent },
  { path: ':id', component: NodeDetailContainerComponent }
];

