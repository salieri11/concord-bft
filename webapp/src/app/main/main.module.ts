import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MainComponentComponent } from './main-component/main-component.component';
import { BlocksModule } from "../blocks/blocks.module";
import { OrgManagementModule } from "../org-management/org-management.module";
import { BlockchainsModule } from "../blockchains/blockchains.module";
import { ChannelsModule } from "../channels/channels.module";
import { HTTP_INTERCEPTORS, HttpClient, HttpClientModule } from "@angular/common/http";
import { TranslateLoader, TranslateModule } from "@ngx-translate/core";
import { HttpLoaderFactory } from "../app.module";
import { RouterModule, Routes } from "@angular/router";
import { ConsortiumManagementModule } from "../consortium-management/consortium-management.module";
import { NodesModule } from "../nodes/nodes.module";
import { TransactionsModule } from "../transactions/transactions.module";
import { SharedModule } from "../shared/shared.module";
import { KubernetesManagementModule } from "../kubernetes-management/kubernetes-management.module";
import { DashboardModule } from "../dashboard/dashboard.module";
import { TestingModule } from "../testing/testing.module";
import { ClarityModule } from "@clr/angular";
import { AuthenticationModule } from "../authentication/authentication.module";
import { ErrorAlertService, GlobalErrorHandlerService } from "../shared/global-error-handler.service";
import { RequestInterceptor } from "../app-interceptors";


const routes: Routes = [
  { path: '',
    redirectTo: 'dashboard',
    pathMatch: 'full'
  }
];

@NgModule({
  imports: [
    CommonModule,
    HttpClientModule,
    RouterModule.forRoot(routes),
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
    TestingModule,
    SharedModule.forRoot(),
    OrgManagementModule,
    BlockchainsModule,
    ConsortiumManagementModule,
    KubernetesManagementModule,
    ChannelsModule,
  ],
  declarations: [MainComponentComponent],
  providers: [
    {
      provide: HTTP_INTERCEPTORS,
      useClass: RequestInterceptor,
      multi: true,
    },
    GlobalErrorHandlerService,
    ErrorAlertService
  ],
})
export class MainModule { }
