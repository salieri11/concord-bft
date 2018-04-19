/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { ActivatedRoute } from '@angular/router';
import { Transaction } from '../../shared/remote-interfaces';

@Component({
  selector: 'app-transaction-detail-container',
  templateUrl: './transaction-detail-container.component.html',
  styleUrls: ['./transaction-detail-container.component.scss']
})
export class TransactionDetailContainerComponent implements OnInit {
  blockNumber: number;
  transaction: Transaction;

  constructor(private httpClient: HttpClient, private route: ActivatedRoute) {}

  ngOnInit() {
    this.route.params.subscribe(params => {
      this.blockNumber = params.blockNumber;
      this.loadTransaction(params.transactionHash);
    });
  }

  loadTransaction(transactionHash) {
    this.httpClient.get(`/api/athena/transactions/${transactionHash}`).subscribe((data: Transaction) => this.transaction = data);
  }
}
