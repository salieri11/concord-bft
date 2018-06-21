import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MainComponent } from './main/main.component';
import { BlocksModule } from "../blocks/blocks.module";
import { OrgManagementModule } from "../org-management/org-management.module";
import { BlockchainsModule } from "../blockchains/blockchains.module";
import { ChannelsModule } from "../channels/channels.module";
import { HTTP_INTERCEPTORS, HttpClientModule } from "@angular/common/http";
import { RouterModule, Routes } from "@angular/router";
import { ConsortiumManagementModule } from "../consortium-management/consortium-management.module";
import { NodesModule } from "../nodes/nodes.module";
import { TransactionsModule } from "../transactions/transactions.module";
import { KubernetesManagementModule } from "../kubernetes-management/kubernetes-management.module";
import { DashboardModule } from "../dashboard/dashboard.module";
import { TestingModule } from "../testing/testing.module";
import { AuthenticationModule } from "../authentication/authentication.module";
import { ErrorAlertService, GlobalErrorHandlerService } from "../shared/global-error-handler.service";
import { RequestInterceptor } from "../app-interceptors";
import { DashboardContainerComponent } from "../dashboard/dashboard-container/dashboard-container.component";
import { SharedModule } from "../shared/shared.module";

const routes: Routes = [
 // { path: 'main', redirectTo: 'dashboard', pathMatch: 'full' }
  { path: '', component: MainComponent, children: [
      { path: 'dashboard', component: DashboardContainerComponent}
    ]}
];

@NgModule({
  imports: [
    CommonModule,
    HttpClientModule,
    SharedModule.forRoot(),
    RouterModule.forChild(routes),
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
