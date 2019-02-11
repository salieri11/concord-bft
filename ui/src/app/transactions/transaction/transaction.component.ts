/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';

import { Transaction } from '../shared/transactions.model';

@Component({
  selector: 'concord-transaction',
  templateUrl: './transaction.component.html',
  styleUrls: ['./transaction.component.scss']
})
export class TransactionComponent implements OnInit {
  blockNumber: number;
  transaction: Transaction;
  transactionHash: string;

  constructor(private route: ActivatedRoute) {}

  ngOnInit() {
    this.route.params.subscribe(params => {
      this.blockNumber = params.blockNumber;
      this.transactionHash = params.transactionHash;
    });
  }
}
