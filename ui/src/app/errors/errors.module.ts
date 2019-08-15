/*
 * Copyright 2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { ForbiddenComponent } from './forbidden/forbidden.component';
import { SharedModule } from './../shared/shared.module';
import { ErrorComponent } from './error/error.component';

@NgModule({
  declarations: [ForbiddenComponent, ErrorComponent],
  imports: [
    SharedModule,
  ]
})
export class ErrorsModule { }
