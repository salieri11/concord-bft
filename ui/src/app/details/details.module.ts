/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { DetailsComponent } from './details/details.component';
import { SharedModule } from '../shared/shared.module';
import { RouterModule } from '@angular/router';
import { SystemComponent } from './system/system.component';

@NgModule({
  imports: [
    CommonModule,
    SharedModule,
    RouterModule,
  ],
  declarations: [
    DetailsComponent,
    SystemComponent,
  ],
  exports: [
    DetailsComponent,
    SystemComponent,
  ]
})
export class DetailsModule { }

