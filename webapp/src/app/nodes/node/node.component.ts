/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';

import { BlockListingBlock } from '../../blocks/shared/blocks.model';
import { TransactionsService } from '../../transactions/shared/transactions.service';

@Component({
  selector: 'athena-node',
  templateUrl: './node.component.html',
  styleUrls: ['./node.component.scss']
})
export class NodeComponent implements OnInit {

  blocks: BlockListingBlock[];
  blockTransactions: any[] = [];
  recentTransactions: any[] = [];

  constructor(private transactionsService: TransactionsService) { }

  ngOnInit() {
    this.transactionsService.getRecentTransactions().subscribe((resp) => {
      this.recentTransactions = resp;
    });
  }
}
