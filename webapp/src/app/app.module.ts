/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { BrowserModule } from '@angular/platform-browser';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { NgModule } from '@angular/core';
import { HttpClient, HttpClientModule } from '@angular/common/http';
import { RouterModule, Routes } from '@angular/router';
import { ClarityModule } from '@clr/angular';
import { TranslateLoader, TranslateModule } from '@ngx-translate/core';
import { TranslateHttpLoader } from '@ngx-translate/http-loader';

import { DashboardModule } from './dashboard/dashboard.module';
import { NodesModule } from './nodes/nodes.module';
import { BlocksModule } from './blocks/blocks.module';
import { TransactionsModule } from './transactions/transactions.module';
import { TestingModule } from './testing/testing.module';

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
    RouterModule.forRoot(appRoutes, {enableTracing: true}),
    TranslateModule.forRoot({
      loader: {
        provide: TranslateLoader,
        useFactory: HttpLoaderFactory,
        deps: [HttpClient]
      }
    }),
    DashboardModule,
    NodesModule,
    BlocksModule,
    TransactionsModule,
    TestingModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
