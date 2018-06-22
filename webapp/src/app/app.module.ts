/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { BrowserModule } from '@angular/platform-browser';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { NgModule } from '@angular/core';
import { HttpClient, HttpClientModule, HTTP_INTERCEPTORS } from '@angular/common/http';
import { RouterModule, Routes } from '@angular/router';
import { ClarityModule } from '@clr/angular';
import { TranslateLoader, TranslateModule } from '@ngx-translate/core';
import { TranslateHttpLoader } from '@ngx-translate/http-loader';

import { AuthenticationModule } from './authentication/authentication.module';
import { DashboardModule } from './dashboard/dashboard.module';
import { NodesModule } from './nodes/nodes.module';
import { BlocksModule } from './blocks/blocks.module';
import { TransactionsModule } from './transactions/transactions.module';
import { TestingModule } from './testing/testing.module';
import { SmartContractsModule } from './smart-contracts/smart-contracts.module';
import { SharedModule } from './shared/shared.module';
import { OrgManagementModule } from './org-management/org-management.module';
import { BlockchainsModule } from './blockchains/blockchains.module';
import { ChannelsModule } from './channels/channels.module';
import { ConsortiumManagementModule } from './consortium-management/consortium-management.module';
import { KubernetesManagementModule } from './kubernetes-management/kubernetes-management.module';
import { GlobalErrorHandlerService, ErrorAlertService } from './shared/global-error-handler.service';

import { RequestInterceptor } from './app-interceptors';

import { AppComponent } from './app.component';

const appRoutes: Routes = [
  { path: '',
    redirectTo: 'dashboard',
    pathMatch: 'full'
  }
];

export function HttpLoaderFactory(http: HttpClient) {
  return new TranslateHttpLoader(http, '/assets/static/i18n/', '.json');
}

@NgModule({
  declarations: [
    AppComponent
  ],
  imports: [
    BrowserModule,
    BrowserAnimationsModule,
    HttpClientModule,
    ClarityModule,
    RouterModule.forRoot(appRoutes),
    TranslateModule.forRoot({
      loader: {
        provide: TranslateLoader,
        useFactory: HttpLoaderFactory,
        deps: [HttpClient]
      }
    }),
    AuthenticationModule,
    DashboardModule,
    NodesModule,
    BlocksModule,
    TransactionsModule,
    SmartContractsModule,
    TestingModule,
    SharedModule.forRoot(),
    OrgManagementModule,
    BlockchainsModule,
    ConsortiumManagementModule,
    KubernetesManagementModule,
    ChannelsModule
  ],
  providers: [
    {
      provide: HTTP_INTERCEPTORS,
      useClass: RequestInterceptor,
      multi: true,
    },
    GlobalErrorHandlerService,
    ErrorAlertService
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }
