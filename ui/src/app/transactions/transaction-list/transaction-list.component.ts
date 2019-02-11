/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, Input, OnInit } from '@angular/core';
import { Transaction } from '../shared/transactions.model';
import { TransactionsService } from '../shared/transactions.service';

@Component({
  selector: 'concord-transaction-list',
  templateUrl: './transaction-list.component.html',
  styleUrls: ['./transaction-list.component.scss']
})
export class TransactionListComponent implements OnInit {
  @Input() transactions: Transaction[] = [];
  @Input() blockNumber?: number;

  constructor(private transactionsService: TransactionsService) { }

  ngOnInit() {
    if (this.transactions.length === 0) {
      this.loadRecentTransActions();
    }
  }

  loadRecentTransActions() {
    this.transactionsService.getRecentTransactions()
    .subscribe((resp) => this.transactions = resp);
  }

}
