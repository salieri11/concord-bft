/*
 * Copyright 2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { ForbiddenComponent } from './forbidden/forbidden.component';
import { SharedModule } from './../shared/shared.module';

@NgModule({
  declarations: [ForbiddenComponent],
  imports: [
    SharedModule,
  ]
})
export class ErrorsModule { }
