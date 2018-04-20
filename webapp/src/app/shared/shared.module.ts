/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';

import { AthenaApiService } from './athena-api.service';
import { ATHENA_API_PREFIX } from './shared.config';

@NgModule({
  imports: [],
  providers: [
    {provide: ATHENA_API_PREFIX, useValue: '/api/athena'},
    AthenaApiService,
  ],
  declarations: []
})
export class SharedModule { }
