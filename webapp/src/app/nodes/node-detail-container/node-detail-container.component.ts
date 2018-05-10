/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';

import { BlockListingBlock } from '../../shared/remote-interfaces';
import { AthenaApiService } from '../../shared/athena-api.service';

@Component({
  selector: 'app-node-detail-container',
  templateUrl: './node-detail-container.component.html',
  styleUrls: ['./node-detail-container.component.scss']
})
export class NodeDetailContainerComponent implements OnInit {

  blocks: BlockListingBlock[];
  blockTransactions: any[] = [];
  recentTransactions: any[] = [];

  constructor(private athenaApiService: AthenaApiService) { }

  ngOnInit() {
    // Get blocks, then get individual block, then build list of recent transactions from the data returned
    // This is temporary until there is an endpoint to fetch recent transactions
    this.athenaApiService.getBlocks().subscribe((resp) => {
      this.blocks = resp.blocks;
      this.fetchAndBuildBlockTransactions();
    });
  }

  fetchAndBuildBlockTransactions() {
    // Temporary solution pending a recent transactions endpoint
    if (this.blocks.length) {
      this.athenaApiService.getBlock(this.blocks[0].number).subscribe((resp) => {
        const transactions: any = resp.transactions;
        transactions.map(x => x.blockNumber = this.blocks[0].number);
        this.blockTransactions = this.blockTransactions.concat(transactions);
        this.blocks.shift();
        this.fetchAndBuildBlockTransactions();
      });
    } else {
      this.fetchAndBuildRecentTransactions();
    }
  }

  fetchAndBuildRecentTransactions() {
    // Temporary solution pending a recent transactions endpoint
    if (this.blockTransactions.length) {
      this.athenaApiService.getTransaction(this.blockTransactions[0].hash).subscribe((resp) => {
        const transaction = {blockNumber: this.blockTransactions[0].blockNumber, ...resp};
        this.recentTransactions.push(transaction);
        this.blockTransactions.shift();
        this.fetchAndBuildRecentTransactions();
      });
    }
  }
}
