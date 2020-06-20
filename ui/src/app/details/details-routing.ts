/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Routes } from '@angular/router';
import { DetailsComponent } from './details/details.component';
import { AuthenticatedGuard } from '../shared/authenticated-guard.service';

export const detailsRoutes: Routes = [
  {
    path: ':detailsTarget', component: DetailsComponent,
    canActivate: [AuthenticatedGuard]
  }, {
    path: '', redirectTo: 'blockchain', pathMatch: 'full'
  }
];

