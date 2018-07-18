/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';
import { Subscription } from 'rxjs';

import { BlockListingBlock } from '../../blocks/shared/blocks.model';
import { TransactionsService } from '../../transactions/shared/transactions.service';
import { BlockchainWizardComponent } from '../../shared/components/blockchain-wizard/blockchain-wizard.component';
import { TaskManagerService, getCompletedSetups, getPendingSetups } from '../../shared/task-manager.service';
import { TourService } from '../../shared/tour.service';

import * as NodeGeoJson from '../features.json';

@Component({
  selector: 'athena-dashboard',
  templateUrl: './dashboard.component.html',
  styleUrls: ['./dashboard.component.scss']
})
export class DashboardComponent implements OnInit, OnDestroy {
  @ViewChild('setupWizard') setupWizard: BlockchainWizardComponent;
  blocks: BlockListingBlock[];
  recentTransactions: any[] = [];
  nodeGeoJson: any = NodeGeoJson;
  taskChange: Subscription;
  transactionListScrollChange: Subscription;
  mapScrollChange: Subscription;
  routerFragmentChange: Subscription;
  mockStats = {
    totalActiveNodes: 28458,
    inactiveNodes: 583,
    overallNodeHealth: .8742123,
    transactionsPerSecond: 4289,
    averageValidationTime: 1.98
  };

  constructor(
    private transactionsService: TransactionsService,
    private taskManager: TaskManagerService,
    private route: ActivatedRoute,
    private translate: TranslateService,
    private tourService: TourService
  ) {
  }

  ngOnInit() {
    this.transactionsService.getRecentTransactions().subscribe((resp) => {
      this.recentTransactions = resp;
    });

    this.routerFragmentChange = this.route.fragment.subscribe(fragment => {
      switch (fragment) {
        case 'deploy':
          this.setupBlockchain();
          break;
        case 'orgTour':
          setTimeout(() => {
            this.tourService.startTour();
          });
          break;
        default:
          // code...
          break;
      }
    });

    this.taskChange = this.taskManager.taskChange.subscribe(() => {
      this.handleTaskChange();
    });

    this.transactionListScrollChange = this.tourService.scrollTransactionListSubjectChanges$.subscribe((scroll) => {
      if (scroll === true) {
        setTimeout(() => {
          const element = document.getElementById('transactionList');
          element.scrollIntoView();
        });
      }
    });

    this.mapScrollChange = this.tourService.scrollMapSubjectChanges$.subscribe((scroll) => {
      if (scroll === true) {
        setTimeout(() => {
          const element = document.getElementById('map');
          element.scrollIntoView();
        });
      }
    });
  }

  onNodeStatusStepNext() {
    this.tourService.scrollToTransactionList();
  }

  onTransactionListStepPrev() {
    this.tourService.scrollToMap();
  }

  ngOnDestroy() {
    this.taskChange.unsubscribe();
    this.transactionListScrollChange.unsubscribe();
    this.mapScrollChange.unsubscribe();
    this.routerFragmentChange.unsubscribe();
  }

  setupBlockchain() {
    this.taskManager.resetTasks();
    this.setupWizard.open();
  }

  onSetupComplete(blockchainInfo: any) {
    this.taskManager.handleBlockchainSetup(blockchainInfo);
  }

  private clearTasks() {
    this.nodeGeoJson.features.forEach((feature) => {
      feature.properties.nodes = [];
    });
  }

  private handleTaskChange() {
    if (getCompletedSetups() !== null && getPendingSetups() !== null) {
      getCompletedSetups().forEach(setup => this.parseBlockchainSetup(setup, 'Healthy'));
      getPendingSetups().forEach(setup => this.parseBlockchainSetup(setup, 'Deploying'));

      if (getPendingSetups().length === 0 && getCompletedSetups().length === 0) {
        this.clearTasks();
      }

      this.nodeGeoJson = { ...this.nodeGeoJson };
    }
  }

  private parseBlockchainSetup(blockchainSetup: any, status: string) {
    blockchainSetup.advancedSettings.publicNodesRegions.forEach((region, index) => {
      this.nodeGeoJson.features.forEach((feature) => {
        if (feature.properties.codeName === region.value) {
          const appliedStatus = (status === 'Healthy' && (index === 1 || index === 3)) ? 'Unhealthy' : status;
          const nodeIndex = feature.properties.nodes.findIndex(n => n.id === blockchainSetup.taskId);
          const node = {
            id: blockchainSetup.taskId,
            consortiumName: blockchainSetup.consortium.name,
            blockchainType: this.translate.instant(`blockchainWizard.blockchainAndConsortium.${blockchainSetup.blockchain.type}`),
            status: appliedStatus
          };

          if (nodeIndex === -1) {
            feature.properties.nodes.push(node);
          } else {
            feature.properties.nodes[nodeIndex] = node;
          }
        }
      });
    });
  }
}
