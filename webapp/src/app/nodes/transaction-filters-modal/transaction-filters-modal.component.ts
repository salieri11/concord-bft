/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, EventEmitter, OnInit, Output } from '@angular/core';

@Component({
  selector: 'app-transaction-filters-modal',
  templateUrl: './transaction-filters-modal.component.html',
  styleUrls: ['./transaction-filters-modal.component.css']
})
export class TransactionFiltersModalComponent implements OnInit {

  @Output() applyFilters: EventEmitter<void> = new EventEmitter<void>();

  isOpened = false;

  constructor() { }

  ngOnInit() {
  }

  open() {
    this.isOpened = true;
  }

  onCancel() {
    this.isOpened = false;
  }

  onApply() {
    this.applyFilters.emit();
    this.isOpened = false;
  }
}
