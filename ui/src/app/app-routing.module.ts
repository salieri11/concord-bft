/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { AgreementGuard } from './shared/agreement-guard.service';
import { AuthenticatedGuard } from './shared/authenticated-guard.service';
import { MarketingComponent } from './marketing/marketing.component';
import { environment } from './../environments/environment';

const appRoutes: Routes = [];

if (!environment.csp) {
  appRoutes.push({ path: '', component: MarketingComponent, canActivate: [AgreementGuard] });
} else {
  appRoutes.push({ path: '', redirectTo: 'dashboard', pathMatch: 'full', canActivate: [AuthenticatedGuard]});
}

@NgModule({
  imports: [
    RouterModule.forRoot(appRoutes)
  ],
  exports: [RouterModule]
})
export class AppRoutingModule {
}
