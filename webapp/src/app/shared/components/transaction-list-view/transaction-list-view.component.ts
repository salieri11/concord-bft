/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, Input, OnInit } from '@angular/core';

import { AthenaApiService } from '../../athena-api.service';

@Component({
  selector: 'app-transaction-list-view',
  templateUrl: './transaction-list-view.component.html',
  styleUrls: ['./transaction-list-view.component.scss']
})
export class TransactionListViewComponent implements OnInit {
  @Input() transactions: any[];

  recentTransactions: any[] = [];

  constructor(private athenaApiService: AthenaApiService) {
  }

  ngOnInit() {
  }

  get RecentTransactions() {
    console.log(this.transactions);
    if (this.transactions === undefined) {
    this.athenaApiService.getRecentTransactions().subscribe((resp) => {
      this.recentTransactions = resp;
    });
    } else {
      this.recentTransactions=this.transactions;
    }
    return this.recentTransactions;
  }

}

