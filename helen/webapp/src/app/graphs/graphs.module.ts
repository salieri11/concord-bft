/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule } from '@angular/router';

import { NgxChartsModule } from '@swimlane/ngx-charts';

import { BlockGraphComponent } from './block-graph/block-graph.component';
import { WorldMapComponent } from './world-map/world-map.component';

@NgModule({
  imports: [
    RouterModule,
    NgxChartsModule
  ],
  declarations: [BlockGraphComponent, WorldMapComponent],
  exports: [BlockGraphComponent, WorldMapComponent]
})
export class GraphsModule { }
