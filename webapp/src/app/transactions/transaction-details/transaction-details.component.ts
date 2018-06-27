/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, Input, OnInit } from '@angular/core';

import { Transaction } from '../../shared/remote-interfaces';
import { AthenaApiService } from '../../shared/athena-api.service';

@Component({
  selector: 'athena-transaction-details',
  templateUrl: './transaction-details.component.html',
  styleUrls: ['./transaction-details.component.scss']
})
export class TransactionDetailsComponent implements OnInit {
  @Input() transactionHash: string;

  transaction: Transaction;
  loading = false;
  constructor(private athenaApiService: AthenaApiService) { }

  ngOnInit() {
    this.loadTransaction(this.transactionHash);
  }

  loadTransaction(transactionHash: string) {
    this.loading = true;
    this.athenaApiService.getTransaction(transactionHash).subscribe((response) => {
      this.transaction = response;
      this.loading = false;
    });
  }
}
