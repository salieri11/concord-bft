/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { HTTP_INTERCEPTORS, HttpClientModule } from '@angular/common/http';

import { MainComponent } from './main/main.component';
import { BlocksModule } from '../blocks/blocks.module';
import { OrgManagementModule } from '../org-management/org-management.module';
import { BlockchainsModule } from '../blockchains/blockchains.module';
import { ChannelsModule } from '../channels/channels.module';
import { ConsortiumManagementModule } from '../consortium-management/consortium-management.module';
import { NodesModule } from '../nodes/nodes.module';
import { TransactionsModule } from '../transactions/transactions.module';
import { KubernetesManagementModule } from '../kubernetes-management/kubernetes-management.module';
import { DashboardModule } from '../dashboard/dashboard.module';
import { TestingModule } from '../testing/testing.module';
import { AuthenticationModule } from '../authentication/authentication.module';
import { ErrorAlertService, GlobalErrorHandlerService } from '../shared/global-error-handler.service';
import { RequestInterceptor } from '../app-interceptors';
import { SharedModule } from '../shared/shared.module';
import { MainRoutingModule } from './main/main-routing.module';
import { SmartContractsModule } from "../smart-contracts/smart-contracts.module";


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
    TestingModule,
    OrgManagementModule,
    BlockchainsModule,
    ConsortiumManagementModule,
    KubernetesManagementModule,
    SmartContractsModule,
    ChannelsModule
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
