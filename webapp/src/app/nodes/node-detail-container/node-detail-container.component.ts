/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';

import { TransactionFiltersModalComponent } from '../transaction-filters-modal/transaction-filters-modal.component';

@Component({
  selector: 'app-node-detail-container',
  templateUrl: './node-detail-container.component.html',
  styleUrls: ['./node-detail-container.component.scss']
})
export class NodeDetailContainerComponent implements OnInit {
  @ViewChild('filterModal') filterModal: TransactionFiltersModalComponent;

  constructor() { }

  ngOnInit() {
  }

  onOpenFilterModal() {
    this.filterModal.open();
  }

  onApplyFilters() {
    // TODO: action on apply filters
  }
}
