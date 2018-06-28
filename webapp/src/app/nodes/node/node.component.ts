/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';

import { BlockListingBlock } from '../../shared/remote-interfaces';
import { AthenaApiService } from '../../shared/athena-api.service';

@Component({
  selector: 'app-node',
  templateUrl: './node.component.html',
  styleUrls: ['./node.component.scss']
})
export class NodeComponent implements OnInit {

  blocks: BlockListingBlock[];
  blockTransactions: any[] = [];
  recentTransactions: any[] = [];

  constructor(private athenaApiService: AthenaApiService) { }

  ngOnInit() {
    this.athenaApiService.getRecentTransactions().subscribe((resp) => {
      this.recentTransactions = resp;
    });
  }
}
