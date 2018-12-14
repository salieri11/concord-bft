/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, Input, OnInit } from '@angular/core';

@Component({
  selector: 'concord-log-heat-map',
  templateUrl: './log-heat-map.component.html',
  styleUrls: ['./log-heat-map.component.scss']
})
export class LogHeatMapComponent implements OnInit {
  @Input('graphData') graphData: any[];

  // graph options
  legend = false;
  showGridLines = true;
  colorScheme = {
    domain: ['#ffffff', '#2a7400']
  };
  constructor() { }

  ngOnInit() {
  }

  tooltipText(cell: any): string {
    return cell.data;
  }

}
