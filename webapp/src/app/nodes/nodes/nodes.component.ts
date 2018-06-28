/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';

import { AthenaApiService } from '../../shared/athena-api.service';

import { Member } from '../../shared/remote-interfaces';

@Component({
  selector: 'athena-nodes',
  templateUrl: './nodes.component.html',
  styleUrls: ['./nodes.component.scss']
})
export class NodesComponent implements OnInit {

  mockStats = {
    totalActiveNodes: 28458,
    inactiveNodes: 583,
    overallNodeHealth: .8742123,
    transactionsPerSecond: 4289,
    averageValidationTime: 1.98
  };

  members: Member[] = [];

  constructor(private athenaApiService: AthenaApiService) {}

  ngOnInit() {
    this.loadMembers();
  }

  loadMembers() {
    this.athenaApiService.getMembers().subscribe(members => this.members = members);
  }
}
