/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Transaction } from '../../shared/remote-interfaces';

@Component({
  selector: 'athena-transaction-detail-container',
  templateUrl: './transaction-detail-container.component.html',
  styleUrls: ['./transaction-detail-container.component.scss']
})
export class TransactionDetailContainerComponent implements OnInit {
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
