/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { HTTP_INTERCEPTORS, HttpClientModule } from '@angular/common/http';
import { TourNgxPopperModule } from 'ngx-tour-ngx-popper';

import { MainComponent } from './main/main.component';
import { BlocksModule } from '../blocks/blocks.module';
import { NodesModule } from '../nodes/nodes.module';
import { TransactionsModule } from '../transactions/transactions.module';
import { DashboardModule } from '../dashboard/dashboard.module';
import { AuthenticationModule } from '../authentication/authentication.module';
import { ErrorAlertService, GlobalErrorHandlerService } from '../shared/global-error-handler.service';
import { RequestInterceptor } from '../app-interceptors';
import { SharedModule } from '../shared/shared.module';
import { MainRoutingModule } from './main/main-routing.module';
import { SmartContractsModule } from '../smart-contracts/smart-contracts.module';
import { OrgsModule } from '../orgs/orgs.module';
import { ConsortiumModule } from '../consortium/consortium.module';
import { UsersModule } from '../users/users.module';


@NgModule({
  imports: [
    CommonModule,
    MainRoutingModule,
    HttpClientModule,
    SharedModule.forRoot(),
    AuthenticationModule,
    DashboardModule,
    NodesModule,
    BlocksModule,
    TransactionsModule,
    OrgsModule,
    ConsortiumModule,
    SmartContractsModule,
    UsersModule,
    TourNgxPopperModule
  ],
  declarations: [MainComponent],
  providers: [
    {
      provide: HTTP_INTERCEPTORS,
      useClass: RequestInterceptor,
      multi: true,
    },
    GlobalErrorHandlerService,
    ErrorAlertService
  ]
})
export class MainModule {
}
