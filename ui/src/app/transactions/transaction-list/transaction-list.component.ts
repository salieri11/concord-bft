/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, Input, OnInit } from '@angular/core';

import { Transaction } from '../shared/transactions.model';
import { TransactionsService } from '../shared/transactions.service';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';

@Component({
  selector: 'concord-transaction-list',
  templateUrl: './transaction-list.component.html',
  styleUrls: ['./transaction-list.component.scss']
})
export class TransactionListComponent implements OnInit {
  @Input() transactions: Transaction[] = [];
  @Input() blockNumber?: number;
  @Input() titleHidden: boolean = false;
  @Input() pagination: number = 20;
  @Input() listOnly: boolean = false;
  @Input() expandAll: boolean = false;
  @Input() noListIfSingle: boolean = false;
  @Input() codeBlockMaxHeight: string;
  blockchainId: string;
  transactionsLoaded: boolean = false;

  constructor(
    private transactionsService: TransactionsService,
    private blockchainService: BlockchainService,
  ) {
    if (!this.pagination) { this.pagination = 20; }
  }

  ngOnInit() {
    if (!this.pagination) { this.pagination = 20; }
    if (!isNaN(this.blockNumber) && this.blockNumber >= 0) { // valid number
      this.loadBlockTransactions();
    } else if (this.transactions.length === 0) {
      this.loadRecentTransActions();
    }

    this.blockchainId = this.blockchainService.blockchainId;
  }

  loadRecentTransActions() {
    this.transactionsService.getRecentTransactions()
      .subscribe((resp) => {
        this.transactions = resp;
        this.transactionsLoaded = true;
      });
  }

  loadBlockTransactions() {
    this.transactionsService.getBlockTransactions(this.blockNumber)
      .subscribe((resp) => {
        this.transactions = resp;
        this.transactionsLoaded = true;
      });
  }

}
