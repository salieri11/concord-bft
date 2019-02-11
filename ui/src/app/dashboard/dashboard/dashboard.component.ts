/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';
import { Subscription } from 'rxjs';

import { BlockListingBlock } from '../../blocks/shared/blocks.model';
import { TransactionsService } from '../../transactions/shared/transactions.service';
import { BlockchainWizardComponent } from '../../shared/components/blockchain-wizard/blockchain-wizard.component';
import { TourService } from '../../shared/tour.service';
import { SmartContractsService } from '../../smart-contracts/shared/smart-contracts.service';
import { BlocksService } from '../../blocks/shared/blocks.service';
import { DashboardListConfig } from '../dashboard-list/dashboard-list.component';
import { NodesService } from '../../nodes/shared/nodes.service';

import * as NodeGeoJson from '../features.json';

const LONG_POLL_INTERVAL = 30000; // Thirty seconds
const BLOCK_TRANSACTION_LIMIT = 20;

@Component({
  selector: 'concord-dashboard',
  templateUrl: './dashboard.component.html',
  styleUrls: ['./dashboard.component.scss']
})
export class DashboardComponent implements OnInit, OnDestroy {
  @ViewChild('setupWizard') setupWizard: BlockchainWizardComponent;
  blocks: BlockListingBlock[] = [];
  transactions: any[] = [];
  nodes: any[] = [];
  smartContracts = [];
  nodeGeoJson: any = NodeGeoJson;
  routerFragmentChange: Subscription;
  firstBlockTransactionCount: number = 0;
  pollIntervalId: any;
  nodeHealth: number = 1;

  constructor(
    private transactionsService: TransactionsService,
    private smartContractsService: SmartContractsService,
    private blocksService: BlocksService,
    private nodesService: NodesService,
    private route: ActivatedRoute,
    private router: Router,
    private translate: TranslateService,
    private tourService: TourService
  ) {
  }

  ngOnInit() {
    this.loadTransactions();
    this.loadSmartContracts();
    this.loadNodes();
    this.loadBlocks();

    this.pollIntervalId = setInterval(() => {
      this.loadTransactions();
      this.loadNodes();
      this.loadBlocks();
      this.loadSmartContracts();
    }, LONG_POLL_INTERVAL);

    this.tourService.initialDashboardUrl = this.router.url.substr(1);

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
  }

  ngOnDestroy() {
    this.routerFragmentChange.unsubscribe();
    clearInterval(this.pollIntervalId);
  }

  get transactionCount() {
    const blockCount = this.blocks[0] ? this.blocks[0].number : 0;
    const firstBlockTransactionCount = this.firstBlockTransactionCount;
    const count = blockCount + firstBlockTransactionCount;
    return count < 0 ? 0 : count;
  }

  get healthyNodesCount() {
    return this.nodes.filter((node) => {
      return node.millis_since_last_message < node.millis_since_last_message_threshold;
    }).length;
  }

  get nodesConfig(): DashboardListConfig {
    return {
      headers: ['nodes.hostname', 'nodes.address', 'nodes.health'],
      displayProperties: ['hostname', 'address', (node) => {
        let text = '';
        let labelClass = '';

        if (node.millis_since_last_message < node.millis_since_last_message_threshold) {
          text = this.translate.instant('nodes.healthy');
          labelClass = 'label-success';
        } else {
          text = this.translate.instant('nodes.unhealthy');
          labelClass = 'label-danger';
        }

        return `<span class="label ${labelClass}">${text}</span>`;
      }],
      tableHeader: 'nodes.nodes',
      paginationSummary: 'nodes.paginationSummary'
    };
  }

  get blocksConfig(): DashboardListConfig {
    return {
      headers: ['blocks.index', 'blocks.hash'],
      displayProperties: ['number', 'hash'],
      tableHeader: 'blocks.blocks',
      itemLink: (block) => {
        return ['/blocks', block.number];
      },
      paginationSummary: 'blocks.paginationSummary'
    };
  }

  get transactionsConfig(): DashboardListConfig {
    return {
      headers: ['transactions.hash', 'transactions.nonce'],
      displayProperties: ['hash', 'nonce'],
      tableHeader: 'transactions.transactions',
      itemLink: (transaction) => {
        return ['/blocks', transaction.block_number, 'transactions', transaction.hash];
      },
      paginationSummary: 'transactions.paginationSummary'
    };
  }

  get contractsConfig(): DashboardListConfig {
    return {
      headers: ['smartContracts.name', 'smartContracts.owner'],
      displayProperties: ['contract_id', 'owner'],
      tableHeader: 'smartContracts.smartContracts',
      itemLink: (contract) => {
        return ['/smart-contracts', contract.contract_id];
      },
      paginationSummary: 'smartContracts.paginationSummary'
    };
  }

  setupBlockchain() {
    this.setupWizard.open();
  }

  private loadSmartContracts() {
    this.smartContractsService.getSmartContracts().subscribe(smartContracts => this.smartContracts = smartContracts);
  }

  private loadNodes() {
    this.nodesService.getNodes().subscribe((resp) => {
      this.nodes = resp;
      this.nodeHealth = this.healthyNodesCount / this.nodes.length;
    });
  }

  private loadBlocks() {
    this.blocksService.getBlocks(BLOCK_TRANSACTION_LIMIT).subscribe((resp) => {
      this.blocks = resp.blocks;
    });
    this.blocksService.getBlock(0).subscribe((resp) => {
      this.firstBlockTransactionCount = resp.transactions.length;
    });
  }

  private loadTransactions() {
    this.transactionsService.getTransactions(BLOCK_TRANSACTION_LIMIT).subscribe((resp) => {
      this.transactions = resp.transactions;
    });
  }
}
