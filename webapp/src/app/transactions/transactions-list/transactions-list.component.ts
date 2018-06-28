/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, Input, OnInit } from '@angular/core';

@Component({
  selector: 'app-transactions-list',
  templateUrl: './transactions-list.component.html',
  styleUrls: ['./transactions-list.component.scss']
})
export class TransactionsListComponent implements OnInit {
  @Input() transactions: any[];
  @Input() blockNumber?: number;

  constructor() { }

  ngOnInit() {
  }

}
