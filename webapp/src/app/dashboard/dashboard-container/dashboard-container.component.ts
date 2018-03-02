/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'app-dashboard-container',
  templateUrl: './dashboard-container.component.html',
  styleUrls: ['./dashboard-container.component.css']
})
export class DashboardContainerComponent implements OnInit {

  mockStats = {
    totalActiveNodes: 28458,
    inactiveNodes: 583,
    overallNodeHealth: .8742123,
    transactionsPerSecond: 4289,
    averageValidationTime: 1.98
  };

  constructor() { }

  ngOnInit() {
  }

}
