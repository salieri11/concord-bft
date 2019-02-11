/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, Input, OnInit } from '@angular/core';

export interface DashboardListConfig {
  headers: string[];
  displayProperties: Array<string|Function>;
  tableHeader: string;
  itemLink?: Function;
  paginationSummary?: string;
}

@Component({
  selector: 'concord-dashboard-list',
  templateUrl: './dashboard-list.component.html',
  styleUrls: ['./dashboard-list.component.scss']
})
export class DashboardListComponent implements OnInit {

  @Input('listConfig') listConfig: DashboardListConfig;
  @Input('items') items: any[];

  constructor() { }

  ngOnInit() {
  }

  getItemProp(item, propName) {
    if (typeof propName === 'function') {
      return propName(item);
    } else {
      return item[propName];
    }
  }
}
