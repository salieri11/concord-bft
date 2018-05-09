/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';

import { AuthenticationService } from './authentication.service';
import { AuthenticatedGuard } from './authenticated-guard.service';
import { AthenaApiService } from './athena-api.service';
import { EthApiService } from './eth-api.service';
import { ATHENA_API_PREFIX, ETHEREUM_API_PREFIX } from './shared.config';


@NgModule({
  providers: [
    AuthenticationService,
    AuthenticatedGuard,
    {provide: ATHENA_API_PREFIX, useValue: '/api/athena'},
    {provide: ETHEREUM_API_PREFIX, useValue: '/api/athena/eth'},
    AthenaApiService,
    EthApiService
  ],
  declarations: []
})
export class SharedModule { }
