/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule, Routes } from '@angular/router';
import { FormsModule } from '@angular/forms';

import { ClarityModule } from '@clr/angular';
import { TranslateModule } from '@ngx-translate/core';

import { SharedModule } from '../shared/shared.module';

import { OnboardingComponent } from './onboarding/onboarding.component';
import { authRoutes } from '../shared/urls.model';

import { AuthenticatedGuard } from '../shared/authenticated-guard.service';

const routes: Routes = [
  // ROUTE: /auth/*
  {
    path: authRoutes.base,
    children: [
      {
        // ROUTE: /auth/onboarding (TOS agreement)
        path: authRoutes.onboarding,
        canActivate: [AuthenticatedGuard],
        component: OnboardingComponent,
      }
    ]
  }
];

@NgModule({
  imports: [
    CommonModule,
    RouterModule.forChild(routes),
    ClarityModule,
    TranslateModule,
    SharedModule,
    FormsModule
  ],
  declarations: [OnboardingComponent]
})
export class AuthenticationModule { }
