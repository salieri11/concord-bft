/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { LoggingComponent } from './logging/logging.component';
import { SharedModule } from '../shared/shared.module';
import { LogDetailsComponent } from './log-details/log-details.component';
import { GraphsModule } from '../graphs/graphs.module';
import { ExportChartDataModalComponent } from './export-chart-data-modal/export-chart-data-modal.component';
import { ExportLogEventsModalComponent } from './export-log-events-modal/export-log-events-modal.component';

@NgModule({
  imports: [
    CommonModule,
    SharedModule,
    GraphsModule
  ],
  declarations: [
    LoggingComponent,
    LogDetailsComponent,
    ExportChartDataModalComponent,
    ExportLogEventsModalComponent
  ]
})
export class LoggingModule { }

