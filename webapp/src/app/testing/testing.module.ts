/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule, Routes } from '@angular/router';
import { ClarityModule } from '@clr/angular';
import { TranslateModule } from '@ngx-translate/core';
import { SharedModule } from '../shared/shared.module';

import { AuthenticatedGuard } from '../shared/authenticated-guard.service';

import { TestingGroundComponent } from './testing-ground/testing-ground.component';

const routes: Routes = [
  {
    path: 'testing',
    canActivateChild: [AuthenticatedGuard],
    children: [
      {path: '', component: TestingGroundComponent}
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
  declarations: [TestingGroundComponent]
})
export class TestingModule { }
