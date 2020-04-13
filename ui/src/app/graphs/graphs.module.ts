/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule } from '@angular/router';

import { NgxChartsModule } from '@swimlane/ngx-charts';
import { SharedModule } from '../shared/shared.module';

import { BlockGraphComponent } from './block-graph/block-graph.component';
import { LogGraphComponent } from './log-graph/log-graph.component';
import { LogHeatMapComponent } from './log-heat-map/log-heat-map.component';
import { WorldMapComponent } from './world-map/world-map.component';
import { WavefrontGraphComponent } from './wavefront-graph/wavefront-graph.component';

@NgModule({
  imports: [
    RouterModule,
    NgxChartsModule,
    SharedModule
  ],
  declarations: [
    BlockGraphComponent,
    WorldMapComponent,
    LogGraphComponent,
    LogHeatMapComponent,
    WavefrontGraphComponent
  ],
  exports: [
    BlockGraphComponent,
    WorldMapComponent,
    LogGraphComponent,
    LogHeatMapComponent,
    WavefrontGraphComponent
  ]
})
export class GraphsModule { }
