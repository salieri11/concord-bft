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
import { OrgsModule } from './orgs/orgs.module';
import { BlockchainsModule } from './blockchains/blockchains.module';
import { ChannelsModule } from './channels/channels.module';
import { GlobalErrorHandlerService, ErrorAlertService } from './shared/global-error-handler.service';

import { RequestInterceptor } from './app-interceptors';

import { AppComponent } from './app.component';
import { ConsortiumModule } from './consortium/consortium.module';
import { KubernetesModule } from './kubernetes/kubernetes.module';

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
    OrgsModule,
    BlockchainsModule,
    ConsortiumModule,
    KubernetesModule,
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
