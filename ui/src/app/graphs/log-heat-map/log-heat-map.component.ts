/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { AfterViewInit, Component, Input, OnInit, ViewChild } from '@angular/core';
import { HeatMapComponent } from '@swimlane/ngx-charts';

@Component({
  selector: 'concord-log-heat-map',
  templateUrl: './log-heat-map.component.html',
  styleUrls: ['./log-heat-map.component.scss']
})
export class LogHeatMapComponent implements OnInit, AfterViewInit {
  @ViewChild(HeatMapComponent, { static: true }) heatMap: HeatMapComponent;
  @Input('graphData') graphData: any[];
  @Input('xAxisLabel') xAxisLabel: string;
  @Input('yAxisLabel') yAxisLabel: string;

  // graph options
  legend = true;
  showGridLines = true;
  colorScheme = {
    domain: ['#eeeeee', '#60B515']
  };
  constructor() { }

  ngAfterViewInit() {
    setTimeout(() => {
      // Allows the heat map to take up some more space
      // top, right, bottom, left
      this.heatMap.margin = [0, 5, 10, 25];
      this.heatMap.update();
    });
  }

  ngOnInit() {
  }

  tooltipText(cell: any): string {
    return cell.data;
  }

}
