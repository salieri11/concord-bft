/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule, Routes } from '@angular/router';
import { ClarityModule } from '@clr/angular';

import { SwaggerComponent } from './swagger/swagger.component';
import { AgreementGuard } from '../shared/agreement-guard.service';
import { AuthenticatedGuard } from '../shared/authenticated-guard.service';
import { SharedModule } from '../shared/shared.module';

const routes: Routes = [
  {
    path: 'docs',
    canActivate: [AgreementGuard],
    canActivateChild: [AuthenticatedGuard],
    children: [
      {
        path: 'api',
        component: SwaggerComponent,
        canActivate: [AgreementGuard],
      }
    ]
  }
];

@NgModule({
  imports: [
    CommonModule,
    RouterModule.forChild(routes),
    ClarityModule,
    SharedModule
  ],
  declarations: [SwaggerComponent]
})
export class SwaggerModule { }
