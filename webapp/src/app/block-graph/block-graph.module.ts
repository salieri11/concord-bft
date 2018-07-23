/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { NgxChartsModule } from '@swimlane/ngx-charts';

import { BlockGraphComponent } from './block-graph/block-graph.component';

@NgModule({
  imports: [
    CommonModule,
    NgxChartsModule
  ],
  declarations: [BlockGraphComponent],
  exports: [BlockGraphComponent]
})
export class BlockGraphModule { }
