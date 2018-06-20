/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Transaction } from '../../shared/remote-interfaces';
import { AthenaApiService } from '../../shared/athena-api.service';

@Component({
  selector: 'athena-transaction-detail-container',
  templateUrl: './transaction-detail-container.component.html',
  styleUrls: ['./transaction-detail-container.component.scss']
})
export class TransactionDetailContainerComponent implements OnInit {
  blockNumber: number;
  transaction: Transaction;

  constructor(private athenaApiService: AthenaApiService, private route: ActivatedRoute) {}

  ngOnInit() {
    this.route.params.subscribe(params => {
      this.blockNumber = params.blockNumber;
      this.loadTransaction(params.transactionHash);
    });
  }

  loadTransaction(transactionHash) {
    this.athenaApiService.getTransaction(transactionHash).subscribe(transaction => this.transaction = transaction);
  }
}
