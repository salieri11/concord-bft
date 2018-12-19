/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule } from '@angular/router';

import { NgxChartsModule } from '@swimlane/ngx-charts';

import { BlockGraphComponent } from './block-graph/block-graph.component';
import { WorldMapComponent } from './world-map/world-map.component';
import { LogGraphComponent } from './log-graph/log-graph.component';
import { LogHeatMapComponent } from './log-heat-map/log-heat-map.component';

@NgModule({
  imports: [
    RouterModule,
    NgxChartsModule
  ],
  declarations: [BlockGraphComponent, WorldMapComponent, LogGraphComponent, LogHeatMapComponent],
  exports: [BlockGraphComponent, WorldMapComponent, LogGraphComponent, LogHeatMapComponent]
})
export class GraphsModule { }
