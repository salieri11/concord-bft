/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule, ModuleWithProviders } from '@angular/core';
import { CommonModule } from '@angular/common';
import { HTTP_INTERCEPTORS, HttpClientModule } from '@angular/common/http';
import { TourNgxPopperModule } from 'ngx-tour-ngx-popper';

import { ZonesModule } from '../zones/zones.module';
import { NodesModule } from '../nodes/nodes.module';
import { DashboardModule } from '../dashboard/dashboard.module';
import { BlockchainModule } from '../blockchain/blockchain.module';
import { AuthenticationModule } from '../authentication/authentication.module';
import { RequestInterceptor } from '../app-interceptors';
import { SharedModule } from '../shared/shared.module';
import { MainRoutingModule } from './main/main-routing.module';
import { OrgsModule } from '../orgs/orgs.module';
import { ConsortiumModule } from '../consortium/consortium.module';
import { TasksModule } from '../tasks/tasks.module';
import { FeaturesModule } from '../features/features.module';
import { EnvironmentModule } from './../environment/environment.module';
import { DetailsModule } from '../details/details.module';

import { ErrorAlertService, GlobalErrorHandlerService } from '../shared/global-error-handler.service';

import { MainComponent } from './main/main.component';
import { WelcomeContentComponent } from './welcome-content/welcome-content.component';
import { DeployingInterstitialComponent } from './deploying-interstitial/deploying-interstitial.component';
import { DeployComponent } from './deploy/deploy.component';
import { WelcomeComponent } from './welcome/welcome.component';
import { DeployingComponent } from './deploying/deploying.component';
import { RouterModule } from '@angular/router';
import { TransactionsModule } from '../transactions/transactions.module';
import { BlocksModule } from '../blocks/blocks.module';

const defaultProvides: any[] = [
  { provide: HTTP_INTERCEPTORS, useClass: RequestInterceptor, multi: true },
  GlobalErrorHandlerService,
  ErrorAlertService
];

@NgModule({
  imports: [
    CommonModule,
    MainRoutingModule,
    RouterModule,
    HttpClientModule,
    SharedModule.forRoot(),
    AuthenticationModule,
    DashboardModule,
    BlockchainModule,
    ZonesModule,
    NodesModule,
    OrgsModule,
    ConsortiumModule,
    TransactionsModule,
    BlocksModule,
    TasksModule,
    FeaturesModule,
    EnvironmentModule,
    TourNgxPopperModule,
    DetailsModule,
  ],
  declarations: [
    MainComponent,
    WelcomeContentComponent,
    DeployingInterstitialComponent,
    DeployComponent,
    WelcomeComponent,
    DeployingComponent,
  ],
})
export class MainModule {
  constructor() {
    if (window['UNIT_TEST_ENV']) { MainModule.forRoot = MainModule.forChild = MainModule.forTesting; }
  }
  public static forRoot(): ModuleWithProviders {
    return { ngModule: MainModule, providers: defaultProvides };
  }
  public static forChild(): ModuleWithProviders {
    return { ngModule: MainModule, providers: defaultProvides };
  }
  public static forTesting(): ModuleWithProviders {
    return { ngModule: SharedModule, providers: [GlobalErrorHandlerService, ErrorAlertService] };
  }
}
