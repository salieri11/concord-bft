/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, Input, OnInit, OnChanges, SimpleChanges } from '@angular/core';

import { Transaction } from '../shared/transactions.model';
import { TransactionsService } from '../shared/transactions.service';

@Component({
  selector: 'concord-transaction-details',
  templateUrl: './transaction-details.component.html',
  styleUrls: ['./transaction-details.component.scss']
})
export class TransactionDetailsComponent implements OnInit, OnChanges {
  @Input() transactionHash: string;

  transaction: Transaction;

  constructor(
    private transactionsService: TransactionsService,
    ) { }

  ngOnInit() {
    this.loadTransaction(this.transactionHash);
  }

  ngOnChanges(changes: SimpleChanges) {
    if (changes.transactionHash && changes.transactionHash.previousValue) {
      this.loadTransaction(changes.transactionHash.currentValue);
    }
  }

  loadTransaction(transactionHash: string) {
    this.transactionsService.getTransaction(transactionHash).subscribe((response) => {
      this.transaction = response;
    });
  }
}
