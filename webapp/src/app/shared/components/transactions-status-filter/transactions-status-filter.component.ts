/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { FormGroup, FormBuilder } from '@angular/forms';
import { ClrDatagridFilterInterface } from '@clr/angular';
import { Subject } from 'rxjs/Subject';

import { Transaction } from '../../remote-interfaces';

@Component({
  selector: 'app-transactions-status-filter',
  templateUrl: './transactions-status-filter.component.html',
  styleUrls: ['./transactions-status-filter.component.scss']
})
export class TransactionsStatusFilterComponent implements OnInit, ClrDatagridFilterInterface<Transaction> {
  private form: FormGroup;
  private options: string[] = [
    '-1',
    '0',
    '1'
  ];
  changes = new Subject<any>();

  constructor(private formBuilder: FormBuilder) {
    this.form = this.formBuilder.group({
      filterOption: ['-1']
    });
  }

  ngOnInit() {
  }

  onOptionChange() {
    this.changes.next(true);
  }
  isActive(): boolean {
    return this.form.controls.filterOption.value !== '-1';
  }
  accepts(transaction: Transaction): boolean {
    return this.form.controls.filterOption.value === '-1' || transaction.status.toString() === this.form.controls.filterOption.value;
  }
}
