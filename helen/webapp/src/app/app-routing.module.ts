/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { AgreementGuard } from './shared/agreement-guard.service';
import { MarketingComponent } from './marketing/marketing.component';

const appRoutes: Routes = [
  { path: '', component: MarketingComponent, canActivate: [AgreementGuard] },
];

@NgModule({
  imports: [
    RouterModule.forRoot(appRoutes)
  ],
  exports: [RouterModule]
})
export class AppRoutingModule {
}
