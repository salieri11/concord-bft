/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { ClrDatagridFilterInterface } from '@clr/angular';
import { Subject } from 'rxjs/Subject';

import { Transaction } from '../../remote-interfaces';

@Component({
  selector: 'app-transactions-status-filter',
  templateUrl: './transactions-status-filter.component.html',
  styleUrls: ['./transactions-status-filter.component.scss']
})
export class TransactionsStatusFilterComponent implements OnInit, ClrDatagridFilterInterface<Transaction> {
  options: string[] = [
    '-1',
    '0',
    '1'
  ];
  selectedOption = '-1';
  changes = new Subject<any>();

  constructor() { }

  ngOnInit() {
  }

  onOptionChange() {
    this.changes.next(true);
  }
  isActive(): boolean {
    return this.selectedOption !== '-1';
  }
  accepts(transaction: Transaction): boolean {
    return this.selectedOption === '-1' || transaction.status.toString() === this.selectedOption;
  }
}
