/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, Input, OnInit } from '@angular/core';

import { Block } from '../../shared/remote-interfaces';
import { AthenaApiService } from '../../shared/athena-api.service';

@Component({
  selector: 'app-block-details',
  templateUrl: './block-details.component.html',
  styleUrls: ['./block-details.component.scss']
})
export class BlockDetailsComponent implements OnInit {
  @Input() blockNumber: number;

  block: Block;
  loading: boolean;

  constructor(private athenaApiService: AthenaApiService) {
  }

  ngOnInit() {
    this.loadBlock(this.blockNumber);
  }

  loadBlock(blockNumber: number) {
    this.loading = true;
    this.athenaApiService.getBlock(blockNumber).subscribe(response => {
      this.block = response;
      this.loading = false;
    });
  }

}
