/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { SharedModule } from '../shared/shared.module';

import { ApiComponent } from './api/api.component';
import { ResourcesComponent } from './resources/resources.component';
import { SwaggerComponent } from './swagger/swagger.component';
import { DappComponent } from './resources/dapp/dapp.component';

@NgModule({
  imports: [
    CommonModule,
    SharedModule,
  ],
  declarations: [
    ApiComponent,
    ResourcesComponent,
    SwaggerComponent,
    DappComponent
  ],
  exports: [
    ApiComponent
  ]
})
export class DeveloperModule { }


