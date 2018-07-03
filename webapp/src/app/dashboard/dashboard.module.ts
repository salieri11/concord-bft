/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule } from '@angular/router';

import { SharedModule } from '../shared/shared.module';
import { TransactionsModule } from '../transactions/transactions.module';
import { DashboardContainerComponent } from './dashboard-container/dashboard-container.component';
import { WorldMapComponent } from './world-map/world-map.component';

@NgModule({
  imports: [
    SharedModule,
    RouterModule,
    TransactionsModule,
  ],
  declarations: [DashboardContainerComponent, WorldMapComponent]
})
export class DashboardModule { }

