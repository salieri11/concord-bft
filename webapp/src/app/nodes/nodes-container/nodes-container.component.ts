/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';
import { HttpClient } from '@angular/common/http';

import { Member } from '../../shared/remote-interfaces';
import { TransactionFiltersModalComponent } from '../transaction-filters-modal/transaction-filters-modal.component';

@Component({
  selector: 'app-nodes-container',
  templateUrl: './nodes-container.component.html',
  styleUrls: ['./nodes-container.component.css']
})
export class NodesContainerComponent implements OnInit {
  @ViewChild('filterModal') filterModal: TransactionFiltersModalComponent;

  mockStats = {
    totalActiveNodes: 28458,
    inactiveNodes: 583,
    overallNodeHealth: .8742123,
    transactionsPerSecond: 4289,
    averageValidationTime: 1.98
  };

  members: Member[] = [];

  constructor(private httpClient: HttpClient) {}

  ngOnInit() {
    this.loadMembers();
  }

  loadMembers() {
    this.httpClient.get('/api/athena/members').subscribe((data: Member[]) => this.members = data);
  }

  onOpenFilterModal() {
    this.filterModal.open();
  }

  onApplyFilters() {
    // TODO: action on apply filters
  }
}
