/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { LoggingComponent } from './logging/logging.component';
import { SharedModule } from '../shared/shared.module';
import { LogDetailsComponent } from './log-details/log-details.component';
import { GraphsModule } from '../graphs/graphs.module';

@NgModule({
  imports: [
    CommonModule,
    SharedModule,
    GraphsModule
  ],
  declarations: [LoggingComponent, LogDetailsComponent]
})
export class LoggingModule { }

