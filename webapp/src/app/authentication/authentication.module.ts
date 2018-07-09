/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule, Routes } from '@angular/router';
import { ClarityModule } from '@clr/angular';
import { TranslateModule } from '@ngx-translate/core';
import { SharedModule } from '../shared/shared.module';

import { LogInContainerComponent } from './log-in-container/log-in-container.component';
import { OnboardingComponent } from './onboarding/onboarding.component';
import { SignUpComponent } from './sign-up/sign-up.component';
import { FormsModule } from '@angular/forms';

const routes: Routes = [
  {
    path: 'auth',
    children: [
      { path: 'login', component: LogInContainerComponent },
      { path: 'onboarding', component: OnboardingComponent },
      { path: 'signup', component: SignUpComponent },
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
