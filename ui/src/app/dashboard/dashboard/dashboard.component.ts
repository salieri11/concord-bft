/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';
import { Subscription } from 'rxjs';

import { GaugeComponent } from '@swimlane/ngx-charts';

import { WorldMapComponent } from '../../graphs/world-map/world-map.component';
import { BlockListingBlock } from '../../blocks/shared/blocks.model';
import { TransactionsService } from '../../transactions/shared/transactions.service';
import { TourService } from '../../shared/tour.service';
import { SmartContractsService } from '../../smart-contracts/shared/smart-contracts.service';
import { BlocksService } from '../../blocks/shared/blocks.service';
import { DashboardListConfig } from '../dashboard-list/dashboard-list.component';
import { NodesService } from '../../nodes/shared/nodes.service';

import * as NodeGeoJson from '../features.json';

const LONG_POLL_INTERVAL = 10000; // Ten seconds
const BLOCK_TRANSACTION_LIMIT = 20;

@Component({
  selector: 'concord-dashboard',
  templateUrl: './dashboard.component.html',
  styleUrls: ['./dashboard.component.scss']
})
export class DashboardComponent implements OnInit, OnDestroy {
  @ViewChild('nodeGuage') nodeGuage: GaugeComponent;
  @ViewChild('worldMap') worldMap: WorldMapComponent;

  blockchainId: string;
  blocks: BlockListingBlock[] = [];
  transactions: any[] = [];
  nodes: any[] = [];
  nodesByLocation: any[] = [];
  smartContracts = [];
  nodeGeoJson: any = NodeGeoJson;
  routerFragmentChange: Subscription;
  firstBlockTransactionCount: number = 0;
  pollIntervalId: any;
  nodeHealth: number = 1;
  nodeColor: string;
  nodeData: { name: string, value: number }[] = [];
  colorScheme = {
    domain: ['#60B515']
  };

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
    this.blockchainId = this.route.snapshot.parent.parent.params['consortiumId'];

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
        case 'orgTour':
          setTimeout(() => {
            this.tourService.startTour();
          });
          break;
        default:
          break;
      }
    });

    this.setNodeData();
    this.nodeGuage.margin = [20, 0, 10, 10];
  }

  ngOnDestroy() {
    if (this.routerFragmentChange) {
      this.routerFragmentChange.unsubscribe();
    }

    clearInterval(this.pollIntervalId);
  }

  private setNodeData() {
    this.nodeData = [{
      name: this.translate.instant('dashboard.nodeHealth'),
      value: this.healthyNodesCount
    }];
  }

  valueFormatting(value) {
    // @ts-ignore
    return `${value}/${this.max}`;
  }

  get transactionCount() {
    const blockCount = this.blocks[0] ? this.blocks[0].number : 0;
    const firstBlockTransactionCount = this.firstBlockTransactionCount;
    const count = blockCount + firstBlockTransactionCount;
    return count < 0 ? 0 : count;
  }

  get healthyNodesCount() {
    const healthyNodeCount = this.nodes.filter((node) => {
      return node.healthy;
    }).length;

    return healthyNodeCount;
  }

  get nodesConfig(): DashboardListConfig {
    return {
      headers: ['nodes.hostname', 'nodes.address', 'nodes.health'],
      displayProperties: ['hostname', 'address', 'healthHTML'],
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
        return [`/${this.blockchainId}`, 'blocks', block.number];
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
        return [`/${this.blockchainId}`, 'blocks', transaction.block_number, 'transactions', transaction.hash];
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
        return [`/${this.blockchainId}`, 'smart-contracts', contract.contract_id];
      },
      paginationSummary: 'smartContracts.paginationSummary'
    };
  }

  private loadSmartContracts() {
    this.smartContractsService.getSmartContracts().subscribe(smartContracts => this.smartContracts = smartContracts);
  }

  private loadNodes() {
    this.nodesService.getNodes().subscribe((resp) => {
      this.nodes = resp.nodes;
      this.nodesByLocation = resp.nodesByLocation;
      this.nodeHealth = this.healthyNodesCount / this.nodes.length;
      this.setNodeData();
      this.setNodeColor();
    });
  }

  setNodeColor() {
    if (this.nodeHealth >= .9) {
      this.nodeColor = 'green';
      this.colorScheme.domain[0] = '#60b515';
    } else if (this.nodeHealth >= .7) {
      this.nodeColor = 'yellow';
      this.colorScheme.domain[0] = '#ffdc0b';
    } else if (this.nodeHealth <= .69) {
      this.nodeColor = 'red';
      this.colorScheme.domain[0] = '#c92100';
    }
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
