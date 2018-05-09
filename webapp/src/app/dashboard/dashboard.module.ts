/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ClarityModule } from '@clr/angular';
import { RouterModule, Routes } from '@angular/router';
import { TranslateModule } from '@ngx-translate/core';
import { SharedModule } from '../shared/shared.module';

import { AuthenticatedGuard } from '../shared/authenticated-guard.service';

import { DashboardContainerComponent } from './dashboard-container/dashboard-container.component';

const routes: Routes = [
  {
    path: 'dashboard',
    canActivateChild: [AuthenticatedGuard],
    children: [
      {path: '', component: DashboardContainerComponent}
    ]
  }
];

@NgModule({
  imports: [
    CommonModule,
    RouterModule.forChild(routes),
    ClarityModule,
    TranslateModule,
    SharedModule
  ],
  declarations: [DashboardContainerComponent]
})
export class DashboardModule { }

