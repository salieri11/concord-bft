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
import { OrgService } from '../../orgs/shared/org.service';
import { TourService } from '../../shared/tour.service';
import { SmartContractsService } from '../../smart-contracts/shared/smart-contracts.service';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { ContractEngines } from '../../blockchain/shared/blockchain.model';
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
  @ViewChild('nodeGuage', { static: false }) nodeGuage: GaugeComponent;
  @ViewChild('worldMap', { static: false }) worldMap: WorldMapComponent;

  blockchainId: string;
  blocks: BlockListingBlock[] = [];
  orgs: any[] = [];
  nodes: any[] = [];
  nodesByLocation: any[] = [];
  onlyOnPrem: boolean = false;
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

  blockchainType: string;
  infoLists: any[] = [];

  dashItems: { title: string, count: number, link: string[] }[];

  constructor(
    private orgService: OrgService,
    private smartContractsService: SmartContractsService,
    private blocksService: BlocksService,
    private nodesService: NodesService,
    private route: ActivatedRoute,
    private router: Router,
    private translate: TranslateService,
    private tourService: TourService,
    private blockchainService: BlockchainService,
  ) { }

  ngOnInit() {
    this.blockchainType = this.blockchainService.type;
    this.setComponents();
    this.loadData();
    this.blockchainId = this.route.snapshot.parent.parent.params['consortiumId'];

    this.pollIntervalId = setInterval(() => {
      this.loadData();
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
    if (this.nodeGuage) {
      this.nodeGuage.margin = [20, 0, 10, 0];
    }
  }

  ngOnDestroy() {
    if (this.routerFragmentChange) {
      this.routerFragmentChange.unsubscribe();
    }

    clearInterval(this.pollIntervalId);
  }

  valueFormatting(value) {
    // @ts-ignore
    return `${value}/${this.max}`;
  }

  get orgCount() {
    return this.orgs.length;
  }

  get transactionCount() {
    const blockCount = this.blocks[0] ? this.blocks[0].number : 0;
    const firstBlockTransactionCount = this.firstBlockTransactionCount;
    const count = blockCount + firstBlockTransactionCount;
    return count < 0 ? 0 : count;
  }

  get currentBlock() {
    return this.blocks[0] ? this.blocks[0].number : 0;
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
      displayProperties: ['name', 'public_ip', 'healthHTML'],
      tableHeader: 'nodes.nodes',
      paginationSummary: 'nodes.paginationSummary',
    };
  }

  get organizationsConfig(): DashboardListConfig {
    return {
      headers: ['organization.columns.name'],
      displayProperties: ['organization_name'],
      tableHeader: 'organization.title',
      paginationSummary: 'organization.paginationSummary',
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
      paginationSummary: 'blocks.paginationSummary',
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
      paginationSummary: 'smartContracts.paginationSummary',
    };
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

  private setNodeData() {
    this.nodeData = [{
      name: this.translate.instant('dashboard.nodeHealth'),
      value: this.healthyNodesCount
    }];
  }

  private loadData() {
    switch (this.blockchainType) {
      case ContractEngines.DAML:
        this.loadOrgs();
        this.loadNodes();
        break;

      case ContractEngines.ETH:
        this.loadOrgs();
        this.loadNodes();
        this.loadBlocks();
        this.loadSmartContracts();
        break;
    }
  }

  private setComponents() {
    switch (this.blockchainType) {
      case ContractEngines.DAML:
        this.dashItems = [{
          title: this.translate.instant('organization.title'),
          link: ['/', this.blockchainId, 'organizations'],
          count: this.orgCount
        }];
        this.infoLists = [
          { config: this.nodesConfig, items: this.nodes, tourAnchor: 'onboardingTour.nodes' },
          { config: this.organizationsConfig, items: this.orgs, tourAnchor: 'onboardingTour.organization' },
        ];
        break;

      case ContractEngines.ETH:
        this.dashItems = [{
          title: this.translate.instant('organization.title'),
          link: ['/', this.blockchainId, 'organizations'],
          count: this.orgCount
        }, {
          title: this.translate.instant('blocks.currentBlock'),
          link: ['/', this.blockchainId, 'blocks'],
          count: this.currentBlock
        }, {
          title: this.translate.instant('dashboard.deployedContracts'),
          link: ['/', this.blockchainId, 'smart-contracts'],
          count: this.smartContracts.length
        }, {
          title: this.translate.instant('transactions.transactions'),
          link: ['/', this.blockchainId, 'transactions'],
          count: this.transactionCount
        }];

        this.infoLists = [
          { config: this.nodesConfig, items: this.nodes, tourAnchor: 'onboardingTour.nodes' },
          { config: this.organizationsConfig, items: this.orgs, tourAnchor: 'onboardingTour.organization' },
          { config: this.contractsConfig, items: this.smartContracts, tourAnchor: 'onboardingTour.smartContracts' },
          { config: this.blocksConfig, items: this.blocks, tourAnchor: 'onboardingTour.blocks' }
        ];
        break;
    }
  }

  private loadSmartContracts() {
    this.smartContractsService.getSmartContracts().subscribe(
      smartContracts => {
        this.smartContracts = smartContracts;
        this.infoLists[2].items = this.smartContracts;
        this.dashItems[2].count = this.smartContracts.length;
      });
  }

  private loadNodes() {
    return this.nodesService.getList().subscribe((resp) => {
      this.nodes = resp.nodes;
      this.nodesByLocation = resp.nodesByLocation;
      this.onlyOnPrem = resp.onlyOnPrem;
      this.nodeHealth = this.healthyNodesCount / this.nodes.length;
      this.setNodeData();
      this.setNodeColor();
      this.infoLists[0].items = this.nodes;
    });
  }

  private loadBlocks() {
    this.blocksService.getBlocks(BLOCK_TRANSACTION_LIMIT).subscribe((resp) => {
      this.blocks = resp.blocks;
      this.dashItems[1].count = this.currentBlock;
      this.dashItems[3].count = this.transactionCount;
      this.infoLists[3].items = this.blocks;
    });
    this.blocksService.getBlock(0).subscribe((resp) => {
      this.firstBlockTransactionCount = resp.transactions.length;
    });
  }

  private loadOrgs() {
    this.orgService.getList().subscribe(resp => {
      this.orgs = resp;
      this.infoLists[1].items = this.orgs;
      this.dashItems[0].count = this.orgCount;
    });
  }

}
