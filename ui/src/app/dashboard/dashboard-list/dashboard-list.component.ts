/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, Input, OnInit } from '@angular/core';

export interface DashboardListConfig {
  headers: string[];
  displayProperties: Array<string|Function>;
  tableHeader: string;
  itemLink?: Function;
  link?: string[];
  paginationSummary?: string;
}

@Component({
  selector: 'concord-dashboard-list',
  templateUrl: './dashboard-list.component.html',
  styleUrls: ['./dashboard-list.component.scss']
})
export class DashboardListComponent implements OnInit {
  @Input('graphConfig') graphConfig: any;
  @Input('timeStart') timeStart: any;
  @Input('timeEnd') timeEnd: any;
  @Input('listConfig') listConfig: DashboardListConfig;
  @Input('items') items: any[];
  @Input('tourAnchor') tourAnchor: string;

  constructor() { }

  ngOnInit() {}

}
