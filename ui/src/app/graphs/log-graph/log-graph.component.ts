/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, Input, OnInit } from '@angular/core';

@Component({
  selector: 'concord-log-graph',
  templateUrl: './log-graph.component.html',
  styleUrls: ['./log-graph.component.scss']
})
export class LogGraphComponent implements OnInit {
  @Input('graphData') graphData: any[];

  // graph options
  showXAxis = true;
  showYAxis = true;
  gradient = true;
  schemeType = 'ordinal';
  showGridLines = true;
  colorScheme = {
    domain: ['#0094d2']
  };

  constructor() { }

  ngOnInit() {
  }

}
