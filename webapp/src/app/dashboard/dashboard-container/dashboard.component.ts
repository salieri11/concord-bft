/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';

import { AthenaApiService } from '../../shared/athena-api.service';
import { BlockListingBlock } from '../../shared/remote-interfaces';

@Component({
  selector: 'app-dashboard',
  templateUrl: './dashboard.component.html',
  styleUrls: ['./dashboard.component.scss']
})
export class DashboardComponent implements OnInit {

  blocks: BlockListingBlock[];
  blockTransactions: any[] = [];
  recentTransactions: any[] = [];
  mockStats = {
    totalActiveNodes: 28458,
    inactiveNodes: 583,
    overallNodeHealth: .8742123,
    transactionsPerSecond: 4289,
    averageValidationTime: 1.98
  };

  constructor(private athenaApiService: AthenaApiService) { }

  ngOnInit() {
    this.athenaApiService.getRecentTransactions().subscribe((resp) => {
      this.recentTransactions = resp;
    });
  }
}
