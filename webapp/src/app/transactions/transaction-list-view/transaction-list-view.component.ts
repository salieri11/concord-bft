/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, Input, OnInit } from '@angular/core';

@Component({
  selector: 'app-transaction-list-view',
  templateUrl: './transaction-list-view.component.html',
  styleUrls: ['./transaction-list-view.component.scss']
})
export class TransactionListViewComponent implements OnInit {
  @Input() transactions: any[];
  @Input() blockNumber?: number;

  constructor() { }

  ngOnInit() {
  }

}
