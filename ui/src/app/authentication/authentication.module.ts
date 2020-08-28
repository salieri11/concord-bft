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
import { LogInContainerComponent } from './login/login.component';

import { OnboardingComponent } from './onboarding/onboarding.component';
import { SignUpComponent } from './sign-up/sign-up.component';
import { authRoutes } from '../shared/urls.model';

import { AgreementGuard } from '../shared/agreement-guard.service';
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
      },
      { // ? DEPRECATED BY CSP; ROUTE: /auth/login (kept for `npm test`)
        path: authRoutes.login,
        canActivate: [AgreementGuard],
        component: LogInContainerComponent
      },
      { // ? DEPRECATED BY CSP; ROUTE: /auth/signup (kept for `npm test`)
        path: authRoutes.signup,
        canActivate: [AgreementGuard],
        component: SignUpComponent
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
  declarations: [LogInContainerComponent, OnboardingComponent, SignUpComponent]
})
export class AuthenticationModule { }
