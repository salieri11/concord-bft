/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule } from '@angular/router';

import { SharedModule } from '../shared/shared.module';
import { TransactionsModule } from '../transactions/transactions.module';
import { DashboardComponent } from './dashboard-container/dashboard.component';
import { WorldMapComponent } from './world-map/world-map.component';

@NgModule({
  imports: [
    SharedModule,
    RouterModule,
    TransactionsModule
  ],
  declarations: [DashboardComponent, WorldMapComponent]
})
export class DashboardModule { }

