/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';
import { ActivatedRoute } from '@angular/router';

import { AthenaApiService } from '../../shared/athena-api.service';
import { BlockListingBlock } from '../../shared/remote-interfaces';
import { VmwTasksService } from '../../shared/components/task-panel/tasks.service';
import { BlockchainSetupWizardComponent } from '../../shared/components/blockchain-setup-wizard/blockchain-setup-wizard.component';

@Component({
  selector: 'athena-dashboard-container',
  templateUrl: './dashboard-container.component.html',
  styleUrls: ['./dashboard-container.component.scss']
})
export class DashboardContainerComponent implements OnInit {
  @ViewChild('setupWizard') setupWizard: BlockchainSetupWizardComponent;

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

  constructor(private athenaApiService: AthenaApiService, private taskService: VmwTasksService, private route: ActivatedRoute) { }

  ngOnInit() {
    this.athenaApiService.getRecentTransactions().subscribe((resp) => {
      this.recentTransactions = resp;
    });

    this.route.fragment.subscribe(fragment => {
      switch (fragment) {
        case 'deploy':
          this.setupBlockchain();
          break;

        default:
          // code...
          break;
      }
    });
  }

  addTask() {
    this.taskService.trackTask({
      title: 'Test',
      description: 'Description',
      failedMessage: 'Something bad happened',
      completedMessage: 'This is done now',
      progress: 25,
      remaining: 100
    });
  }

  setupBlockchain() {
    this.setupWizard.open();
  }

  onSetupComplete(blockchainInfo) {
    console.log(blockchainInfo);
  }
}
