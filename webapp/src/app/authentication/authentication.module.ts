/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ReactiveFormsModule } from '@angular/forms';
import { RouterModule, Routes } from '@angular/router';
import { ClarityModule } from '@clr/angular';
import { TranslateModule } from '@ngx-translate/core';
import { SharedModule } from '../shared/shared.module';

import { LogInContainerComponent } from './log-in-container/log-in-container.component';

const routes: Routes = [
  {
    path: 'auth',
    children: [
      {path: 'log-in', component: LogInContainerComponent},
    ]
  }
];

@NgModule({
  imports: [
    CommonModule,
    ReactiveFormsModule,
    RouterModule.forChild(routes),
    ClarityModule,
    TranslateModule,
    SharedModule
  ],
  declarations: [LogInContainerComponent]
})
export class AuthenticationModule { }
