/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { mainRoutes } from './shared/urls.model';
import { ForbiddenComponent } from './errors/forbidden/forbidden.component';
import { AuthenticatedGuard } from './shared/authenticated-guard.service';
import { ErrorComponent } from './errors/error/error.component';


const appRoutes: Routes = [
  // General, 1st level
  {
    // ROUTE: /forbidden
    path: mainRoutes.forbidden,
    component: ForbiddenComponent,
    canActivate: [AuthenticatedGuard],
  }, {
    // ROUTE: /error
    path: mainRoutes.error,
    component: ErrorComponent,
    canActivate: [AuthenticatedGuard],
  }
];

@NgModule({
  imports: [
    RouterModule.forRoot(appRoutes)
  ],
  exports: [RouterModule]
})
export class AppRoutingModule {
}
