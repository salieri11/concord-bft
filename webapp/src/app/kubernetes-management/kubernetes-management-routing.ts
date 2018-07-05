/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Routes } from '@angular/router';

import { KubernetesManagementComponent } from './kubernetes-management.component';

export const kubernetesManagementRoutes: Routes = [
  { path: '', component: KubernetesManagementComponent },
];
