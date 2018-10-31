/*
 * Copyright 2018 VMware, all rights reserved.
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
import { AgreementGuard } from '../shared/agreement-guard.service';

const routes: Routes = [
  {
    path: 'auth',

    children: [
      {
        path: 'login',
        component: LogInContainerComponent,
        canActivate: [AgreementGuard],
      },
      {
        path: 'signup',
        component: SignUpComponent,
        canActivate: [AgreementGuard],
      },
      {
        path: 'onboarding',
        component: OnboardingComponent
      },
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
