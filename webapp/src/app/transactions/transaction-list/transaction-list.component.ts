/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, Input, OnInit } from '@angular/core';
import { Transaction } from '../shared/transactions.model';

@Component({
  selector: 'athena-transaction-list',
  templateUrl: './transaction-list.component.html',
  styleUrls: ['./transaction-list.component.scss']
})
export class TransactionListComponent implements OnInit {
  @Input() transactions: Transaction[];
  @Input() blockNumber?: number;

  constructor() { }

  ngOnInit() {
  }

}
