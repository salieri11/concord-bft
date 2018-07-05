/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, Input, OnInit } from '@angular/core';

import { Transaction } from '../shared/transactions.model';
import { TransactionsService } from '../shared/transactions.service';

@Component({
  selector: 'athena-transaction-details',
  templateUrl: './transaction-details.component.html',
  styleUrls: ['./transaction-details.component.scss']
})
export class TransactionDetailsComponent implements OnInit {
  @Input() transactionHash: string;

  transaction: Transaction;

  constructor(private transactionsService: TransactionsService) { }

  ngOnInit() {
    this.loadTransaction(this.transactionHash);
  }

  loadTransaction(transactionHash: string) {
    this.transactionsService.getTransaction(transactionHash).subscribe((response) => {
      this.transaction = response;
    });
  }
}
