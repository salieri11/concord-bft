/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ClarityModule } from '@clr/angular';
import { RouterModule, Routes } from '@angular/router';

import { DashboardContainerComponent } from './dashboard-container/dashboard-container.component';

const routes: Routes = [
  {path: 'dashboard', component: DashboardContainerComponent}
];

@NgModule({
  imports: [
    CommonModule,
    RouterModule.forChild(routes),
    ClarityModule
  ],
  declarations: [DashboardContainerComponent]
})
export class DashboardModule { }

